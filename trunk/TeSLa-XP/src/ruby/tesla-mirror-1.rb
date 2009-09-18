# TODO эта версия корректно работает только со случаями Cached >< Mapped!

# TODO сделать поддержку нового формата XML-описаний (шаблонов, тестовых ситуаций, состояний)

require "rexml/document"

class DataBuilder
  
  def initialize
    @tagsets = Hash.new
    dataxml = REXML::Document.new File.new(ARGV[1])
    dataxml.elements.each("data/cache/set"){ |set|
        set.elements.each_with_index{ |tag,delta|
              tagvalue = tag.attributes["value"]
              @tagsets[tagvalue.to_i * 128 + set.attributes["value"].to_i ] = delta }
    }
    
    @micropfns = dataxml.elements.collect("data/tlb/microtlb/pfn"){ |pfn|
                      pfn.attributes["value"].to_i }
    
    @pfns = dataxml.elements.collect("data/tlb/content/line"){ |line|
                      line.attributes["pfn0"].to_i } + 
            dataxml.elements.collect("data/tlb/content/line"){ |line|
                      line.attributes["pfn1"].to_i }
    
    @nonmicropfns = @pfns - @micropfns # в общем случае неверно -- надо допускать пересечения
    
    @tlb = []
    dataxml.elements.each("data/tlb/content/line") {|line|
        tlbline = TLBLine.new
        tlbline.r = line.attributes["range"].to_i
        tlbline.vpndiv2 = line.attributes["vpndiv2"].to_i
        tlbline.mask = line.attributes["mask"].to_i
        tlbline.pfn0 = line.attributes["pfn0"].to_i
        tlbline.CCA0 = line.attributes["CCA0"]
        tlbline.pfn1 = line.attributes["pfn1"].to_i
        tlbline.CCA1 = line.attributes["CCA1"]
        @tlb << tlbline
    }
  end
  
  def LinterPFN
    @LinterPFN ||= @tagsets.select{|tagset,delta|
                    @pfns.include?(tagset/128) }
  end
  
  def L
    @tagsets
  end
  
  def M
     @pfns
  end
  
  def PFNminusM
    @nonmicropfns
  end
  
  def LinterM
     @LinterM ||= @tagsets.select{|tagset,delta|
                    @micropfns.include?(tagset/128) }
  end
  
  def LinterPFNminusM
     @LinterPFNminusM ||= @tagsets.select{|tagset,delta|
                    @nonmicropfns.include?(tagset/128) }
  end
  
  def TLB
    @tlb
  end
end

class TLBLine
  attr_accessor :r
  attr_accessor :vpndiv2
  attr_accessor :mask
  attr_accessor :pfn0
  attr_accessor :CCA0
  attr_accessor :pfn1
  attr_accessor :CCA1
end

class Instruction
  attr_accessor :virtual_address
  attr_accessor :tagset
  attr_accessor :phys_after_translation
  attr_accessor :phys_before_memaccess
  attr_accessor :data
end

class MemoryAccess
  attr_accessor :dwPhysAddr
  attr_accessor :type
  attr_accessor :data
end

class Solver

def initialize
  @unique_counter = 0
  @TAGSETLEN = 31
  @PFNLEN = 24
  @TAGSETTYPE = "BitVec[#{@TAGSETLEN}]"
  @L1ASSOC = 4
  @TLBASSOC = 4
  @SEGBITS = 40
  @PABITS = 36
  @MASK = 0
end

def getPfn(tagset)
  "(extract[30:7] #{tagset})"
end

def getRegion(tagset)
  "(extract[6:0] #{tagset})"
end

def l1Hit( init_tagsets, previous_tagsets, current_tagset )
  mirror init_tagsets, previous_tagsets, current_tagset, "bvugt",\
      (Math.log([@L1ASSOC, init_tagsets.length + previous_tagsets.length].max + 1)/Math.log(2)).ceil
end

def l1Miss( init_tagsets, previous_tagsets, current_tagset )
  mirror init_tagsets, previous_tagsets, current_tagset, "bvule",\
      (Math.log([@L1ASSOC, init_tagsets.length + previous_tagsets.length].max + 1)/Math.log(2)).ceil
end

def mirror( init_tagsets, previous_tagsets, current_tagset, mirrrelation, sumlength )
  puts "(and "
    puts "(or "
          (init_tagsets + previous_tagsets).each{|t|
              puts "(= #{t} #{current_tagset})"  }
    puts ")"
    
    puts "(#{mirrrelation} bv#{@L1ASSOC}[#{sumlength}] (bvadd "
          
          # u(t_i)
          (0.. init_tagsets.length-1).each{|i|
            puts "(ite (and "
               init_tagsets[i..init_tagsets.length-1].each{|t|
                      puts "(= bit0 (bvcomp #{t} #{current_tagset}))" } 
               previous_tagsets.each{|t|
                      puts "(= bit0 (bvcomp #{t} #{current_tagset}))" }
               puts "(= #{getRegion init_tagsets[i]} #{getRegion current_tagset}))" +
            " bv1[#{sumlength}] bv0[#{sumlength}] ) " }
            
          # u(x_i): S_i = miss/hit
          (0.. previous_tagsets.length-1).each{|i|
            puts "(ite (and "
               previous_tagsets[i..previous_tagsets.length-1].each{|t|
                      puts "(= bit0 (bvcomp #{t} #{current_tagset}))"
               }
               puts "(= #{getRegion previous_tagsets[i]} #{getRegion current_tagset})"
               
               if @l1Hits.include?( previous_tagsets[i])
                (0..init_tagsets.length-1).each{|j|
                  puts "(or "
                    # c(t_j) = 0
                    init_tagsets[j..init_tagsets.length-1].each{|t|
                        puts "(= #{current_tagset} #{t})" }
                    previous_tagsets[0..i-1].each{|t|
                        puts "(= #{current_tagset} #{t})" }
                    puts "(= bit0 (bvcomp #{previous_tagsets[i]} #{init_tagsets[j]})))"
                }
                (0..i-1).each{|j|
                  puts "(or "
                    # c(t_j) = 0
                    previous_tagsets[j..i-1].each{|t|
                        puts "(= #{current_tagset} #{t})" }
                    puts "(= bit0 (bvcomp #{previous_tagsets[i]} #{previous_tagsets[j]})))"
                }
               end
                
            puts " ) bv1[#{sumlength}] bv0[#{sumlength}] ) " }
          
      puts " )))"
end

def mtlbHit(previous_tagsets, current_tagset)  
    "(or " +
      @data_builder.M.collect{|m|
            "(and " +
              "(= #{getPfn current_tagset} bv#{m}[#{@PFNLEN}]) " +
               tlb_pfn_is_not_displaced_yet(previous_tagsets, current_tagset, m) + ")"
          }.join + 
    
      previous_tagsets.collect { |tagset|
          " (= #{getPfn current_tagset} #{getPfn tagset}) " }.join + ")"
end

def mtlbMiss(previous_tagsets, current_tagset)  
  "(and " +
    "(or " +
      @data_builder.PFNminusM.collect{|m|
        "(= #{getPfn current_tagset} bv#{m}[#{@PFNLEN}]) "
      }.join +
      @data_builder.M.collect{|m|
            "(and " +
              "(= #{getPfn current_tagset} bv#{m}[#{@PFNLEN}]) " +
               tlb_pfn_is_displaced_already(previous_tagsets, current_tagset, m) + ")"
          }.join + 
    ")" +    
     previous_tagsets.collect { |tagset|
          " (= bit0 (bvcomp #{getPfn current_tagset} #{getPfn tagset})) " }.join + ")"
end

def tlb_pfn_is_not_displaced_yet(previous_tagsets, current_tagset, m)
	delta_T = @data_builder.M.index(m) + 1
  
  if @TLBASSOC - delta_T - previous_tagsets.length > 0
    "true"
  elsif @TLBASSOC - delta_T - (previous_tagsets.length - @mtlbHits.length) <= 0
    "false"
  else
     sumlength = (Math.log(@TLBASSOC - delta_T - (previous_tagsets.length - @mtlbHits.length) + 1) / Math.log(2)).ceil
      "(bvuge bv#{@TLBASSOC - delta_T - previous_tagsets.length + @mtlbHits.length}[#{sumlength}] (bvadd bv0[#{sumlength}] " +
            (0..previous_tagsets.length-1).collect{|i| if @mtlbHits.include? previous_tagsets[i]
            "(and " +
                "(or " +
                  @data_builder.M[delta_T..@TLBASSOC-1].collect{|mtail|
                    "(= #{getPfn previous_tagsets[i]} bv#{mtail}[#{@PFNLEN}]) " }.join +
                ")" +
                
                previous_tagsets[0..i-1].collect{|t|
                  "(= bit0 (bvcomp #{getPfn previous_tagsets[i]} #{getPfn t} )) " }.join +
            ")"
          end }.compact.collect{|f| " (ite #{f} bv1[#{sumlength}] bv0[#{sumlength}])"}.join +                     
      "))"
  end
end

def tlb_pfn_is_displaced_already(previous_tagsets, current_tagset, m)
	delta_T = @data_builder.M.index(m) + 1
	mtlbmissc = previous_tagsets.length - @mtlbHits.length
  
  if @TLBASSOC - delta_T - mtlbmissc <= -1
    "true"
  elsif @TLBASSOC - delta_T - previous_tagsets.length >= 0
    "false"
  else
     sumlength = (Math.log(@TLBASSOC - delta_T - (previous_tagsets.length - @mtlbHits.length) + 1) / Math.log(2)).ceil
      "(bvult bv#{@TLBASSOC - delta_T - previous_tagsets.length + @mtlbHits.length}[#{sumlength}] (bvadd bv0[#{sumlength}] " +
            (0..previous_tagsets.length-1).collect{|i| if @mtlbHits.include? previous_tagsets[i]
            "(and " +
                "(or " +
                  @data_builder.M[delta_T..@TLBASSOC-1].collect{|mtail|
                    "(= #{getPfn previous_tagsets[i]} bv#{mtail}[#{@PFNLEN}]) " }.join +
                ")" +
                
                previous_tagsets[0..i-1].collect{|t|
                  "(= bit0 (bvcomp #{getPfn previous_tagsets[i]} #{getPfn t} )) " }.join +
            ")"
          end }.compact.collect{|f| " (ite #{f} bv1[#{sumlength}] bv0[#{sumlength}])"}.join +                     
      "))"    
  end
end

def process_instruction(instruction, ins_object)
  path = instruction.attributes['name'] +
  "/" + instruction.elements['situation'].attributes['name'] + ".xml"
  test_situation = REXML::Document.new File.new(path)  

  reverse_synonyms = Hash[*test_situation.get_elements('situation/argument').  ## перевести в текст??
      zip(instruction.get_elements('argument')).flatten]
      
  test_situation.elements.each('//[@state="result"]'){|arg|
       new_name = "#{@synonyms[reverse_synonyms[arg].text]}_X"
       puts ":extrafuns (( #{new_name} BitVec[#{arg.attributes['length']}] ))"
  }

  full_context = Hash.new
  @lengths_context = Hash.new
  reverse_synonyms.each{|tsarg, insarg|
      full_context[tsarg.attributes['name']] = @synonyms[insarg.text] +(tsarg.attributes["state"] == "result" ? "_X" : "" )
      @lengths_context[tsarg.attributes['name']] = @varlengths[insarg.text]
  }
  full_context.merge! @synonyms
  @lengths_context.merge! @varlengths
  @ins_object = ins_object
  
  # traverse operators
  test_situation.elements.each('situation/*[not(starts-with(name(),"argument"))]'){|operator|
      send( "constraintsfrom_#{operator.name}", operator, full_context )
  }
  
  test_situation.elements.each('//[@state="result"]'){|arg|
       @synonyms[reverse_synonyms[arg].text] = "#{@synonyms[reverse_synonyms[arg].text]}_X"
  }
end

def constraintsfrom_assert( operator, full_context )
  puts ":assumption"
  puts send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) 
end

def constraintsfrom_and( operator, full_context )
  "(and " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) +
      send("constraintsfrom_#{operator.elements[2].name}", operator.elements[2], full_context) + ")"
end

def constraintsfrom_or( operator, full_context )
  "(or " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) +
      send("constraintsfrom_#{operator.elements[2].name}", operator.elements[2], full_context) + ")"
end

def constraintsfrom_predicate( operator, full_context )
  if operator.attributes['name'] == "wordvalue"
     "(= (extract[63:32] #{full_context[operator.text]}) (repeat[32] (extract[31:31] #{full_context[operator.text]})))"
  else
    raise "unknown predicate \'#{operator.attributes['name']}\'"
  end
end

def constraintsfrom_noteq( operator, full_context )
  "(= bit0 (bvcomp " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) +
      send("constraintsfrom_#{operator.elements[2].name}", operator.elements[2], full_context) + "))"
end

def constraintsfrom_eq( operator, full_context )
  "(= " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) +
      send("constraintsfrom_#{operator.elements[2].name}", operator.elements[2], full_context) + ")"
end

def constraintsfrom_bit( operator, full_context )
  "(extract[#{operator.attributes['index']}:#{operator.attributes['index']}] " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) + ")"
end

def constraintsfrom_bits( operator, full_context )
  "(extract[#{operator.attributes['end']}:#{operator.attributes['start']}] " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) + ")"
end

def constraintsfrom_concat( operator, full_context )
  "(concat " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) +
      send("constraintsfrom_#{operator.elements[2].name}", operator.elements[2], full_context) + ")"
end

def constraintsfrom_power( operator, full_context )
  "(repeat[#{operator.attributes['index']}] " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) + ")"
end

def constraintsfrom_sum( operator, full_context )
  "(bvadd " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) +
      send("constraintsfrom_#{operator.elements[2].name}", operator.elements[2], full_context) + ")"
end

def constraintsfrom_sign_extend( operator, full_context )
  length = send("length_#{operator.elements[1].name}", operator.elements[1])
  "(sign_extend[#{operator.attributes['size'].to_i - length }] " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) + ")"
end

def constraintsfrom_var( operator, full_context )
  v = full_context[operator.text]
  raise "cannot find synonym for variable '#{operator.text}'" if v == nil
  v
end

def constraintsfrom_constant( operator, full_context )
  "bv#{operator.text}[#{operator.attributes['length']}]"
end

def constraintsfrom_bytes_select( operator, full_context )
  new_name = "_localvar_#{@unique_counter += 1}"
  full_context[operator.attributes['name']] = new_name
  
  bitlen = send("length_#{operator.attributes['type']}" )
  @lengths_context[operator.attributes['name']] = bitlen
  puts ":extrafuns (( #{new_name} BitVec[#{bitlen}] ))"
  puts ":assumption"

  if operator.attributes['type'] == "WORD"
     puts "(ite (= bv0[3] #{full_context[operator.elements['index'].text]}) " + 
        "(= #{new_name} (extract[31:0] #{full_context[operator.elements['content'].text]}))" +
        "(= #{new_name} (extract[63:32] #{full_context[operator.elements['content'].text]}))" + ")"
  elsif operator.attributes['type'] == "DOUBLEWORD"
     puts "(= #{new_name} #{full_context[operator.elements['content'].text]})"
  #TODO elsif остальные типы
  else
    raise "unknown bytes-select type '#{operator.attributes['type']}'"
  end
end

def constraintsfrom_bytes_expand( operator, full_context )
  new_name = "_localvar_#{@unique_counter += 1}"
  full_context[operator.attributes['name']] = new_name
  
  @lengths_context[operator.attributes['name']] = 64
  puts ":extrafuns (( #{new_name} BitVec[64] ))"
  puts ":extrafuns (( #{@ins_object.data} BitVec[64] ))"
  puts ":assumption"

  if operator.attributes['type'] == "BYTE"
    (0..7).each{|number|
        puts "(ite (= #{full_context[operator.elements['index'].text]} bv#{number}[3])" +
          "(= #{new_name} (concat (concat bv0[#{56-8*number}] (extract[7:0] #{full_context[operator.elements['content'].text]})) bv0[#{8*number}]))"
    }
    puts "false"
    puts (1..8).collect{")"}.join
  elsif operator.attributes['type'] == "DOUBLEWORD"
    puts "(= #{new_name} #{full_context[operator.elements['content'].text]})"
  #TODO elsif сделать остальные типы
  else
    raise "unknown bytes-select type '#{operator.attributes['type']}'"
  end
end

def constraintsfrom_let( operator, full_context )
  bitlen = send("length_#{operator.elements[1].name}", operator.elements[1] )
  new_name = "_localvar_#{@unique_counter += 1}"
  full_context.merge!( {operator.attributes['name'] => new_name } )
  
  puts ":extrafuns (( #{new_name} BitVec[#{bitlen}] ))"
  puts ":assumption"
  puts "(= #{new_name} " + 
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) + ")"
end

def constraintsfrom_procedure( operator, full_context )
  if operator.attributes['name'] == "AddressTranslation"
    @lengths_context.merge!({operator.elements['physical'].text => @PABITS})
    
    # get physical var from instructions repository
    physical = @ins_object.phys_after_translation
    full_context[operator.elements['physical'].text] = physical
    constraintsfrom_AddressTranslation(operator, full_context)
  elsif operator.attributes['name'] == "LoadMemory"
    @lengths_context[operator.elements[1].text] = 64
    
    data = @ins_object.data
    full_context[operator.elements[1].text] = data
    constraintsfrom_LoadMemory(operator, full_context)
  elsif operator.attributes['name'] == "StoreMemory"
    constraintsfrom_StoreMemory(operator, full_context)
  else
    raise "unknown procedure '#{operator.attributes['name']}'"
  end
end

def length_equalbinary(operator)
  len1 = send("length_#{operator.elements[1].name}", operator.elements[1])
  len2 = send("length_#{operator.elements[2].name}", operator.elements[2])
  raise "arguments must have equal bitlengths (now #{len1} and #{len2})" if len1 != len2
  len1  
end

def length_sum(operator)
  length_equalbinary(operator)
end

def length_concat(operator)
  send("length_#{operator.elements[1].name}", operator.elements[1]) +
  send("length_#{operator.elements[2].name}", operator.elements[2])
end

def length_power(operator)
  send("length_#{operator.elements[1].name}", operator.elements[1]) *
    operator.attributes['index'].to_i
end

def length_bit(operator)
  1
end

def length_bits(operator)
  operator.attributes['end'].to_i - operator.attributes['start'].to_i + 1
end

def length_sign_extend(operator)
  operator.attributes['size'].to_i
end

def length_var(operator)
  l = @lengths_context[operator.text]
  raise "unknown length for '#{operator.text}'" if l == nil
  l
end

def length_DOUBLEWORD
  64
end

def length_WORD
  32
end

def length_HALFWORD
  16
end

def length_BYTE
  8
end

def constraintsfrom_AddressTranslation( operator, full_context )
  puts ":extrafuns(( #{@ins_object.virtual_address} BitVec[64] ))"
  puts ":extrafuns(( #{@ins_object.phys_after_translation} BitVec[#{@PABITS}] ))"
  
  puts ":assumption"
  puts "(= #{@ins_object.virtual_address} #{full_context[operator.elements['virtual'].text]})"
  
  puts ":assumption"
  puts "(= #{@ins_object.phys_after_translation} #{full_context[operator.elements['physical'].text]})"
  
  # модель виртуальной памяти
  #TODO можно сделать более полный Cached Mapped сегмент
  puts ":assumption"
  puts "(= bv0[33] (extract[63:31] #{@ins_object.virtual_address}))"  

  # соответствие некоторой строке TLB
  pfn_name = "_localvar_#{@unique_counter += 1}"
  vpndiv2_name = "_localvar_#{@unique_counter += 1}"
  puts ":assumption"
  puts "(let (#{pfn_name} #{getPfn( @ins_object.tagset )})"
  puts "(let (#{vpndiv2_name} (extract[#{@SEGBITS-1}:#{@PABITS-@PFNLEN}] #{@ins_object.virtual_address}))"
  
  puts "(or "
  @data_builder.TLB.select{|l| l.mask == @MASK && l.r == 0}.each{|tlbline|
    if tlbline.CCA0 == "Cached"
      puts "(and (= #{pfn_name} bv#{tlbline.pfn0}[#{@PFNLEN}])" +
          "(= #{vpndiv2_name} bv#{tlbline.vpndiv2 * 2}[#{@SEGBITS-@PABITS+@PFNLEN}]))"
    end
    if tlbline.CCA1 == "Cached"
      puts "(and (= #{pfn_name} bv#{tlbline.pfn1}[#{@PFNLEN}])" +
          "(= #{vpndiv2_name} bv#{tlbline.vpndiv2 * 2 + 1}[#{@SEGBITS-@PABITS+@PFNLEN}]))"
    end
  }
  puts")))"
    
  # определение physical_after_translation
  puts ":assumption"
  puts "(= #{@ins_object.phys_after_translation} " + 
    "(concat (extract[#{@TAGSETLEN-1}:#{@TAGSETLEN-@PFNLEN}] #{@ins_object.tagset}) " + 
            "(extract[#{@PABITS-@PFNLEN-1}:0] #{@ins_object.virtual_address}) ) )"
            
  # TODO (это только частный случай) часть виртуального адреса -- это часть тегсета (сет)
  puts ":assumption"
  puts "(= (extract[11:5] #{@ins_object.virtual_address}) (extract[6:0] #{@ins_object.tagset}))"
end

def constraintsfrom_LoadMemory( operator, full_context )
  puts ":extrafuns(( #{@ins_object.data} BitVec[64] ))"
  ma = MemoryAccess.new
  ma.dwPhysAddr = "_localvar_#{@unique_counter += 1}"
  ma.type = "LOAD"
  ma.data = @ins_object.data
  puts ":extrafuns(( #{ma.dwPhysAddr} BitVec[#{@PABITS-3}] ))"
  
  puts ":assumption"
  puts "(= #{@ins_object.data} #{full_context[operator.elements['data'].text]})"
  
  puts ":assumption"
  puts "(and true "
  @memory_accesses.reverse.each{|maccess|
      if maccess.type == "LOAD"
          puts "(implies (= #{maccess.dwPhysAddr} #{ma.dwPhysAddr}) (= #{@ins_object.data} #{ma.data}))"
      else
          puts "(ite (= #{maccess.dwPhysAddr} #{ma.dwPhysAddr}) (= #{@ins_object.data} #{ma.data}) (and true "
      end
  }
  @memory_accesses.select{|m| m.type=="STORE"}.each{|m| puts "))" }
  puts ")"
  
  @memory_accesses << ma
end

def constraintsfrom_StoreMemory( operator, full_context )
  ma = MemoryAccess.new
  ma.dwPhysAddr = "_localvar_#{@unique_counter += 1}"
  ma.type = "STORE"
  ma.data = @ins_object.data
  puts ":extrafuns(( #{ma.dwPhysAddr} BitVec[#{@PABITS-3}] ))"
  
  puts ":assumption"
  puts "(= #{@ins_object.data} #{full_context[operator.elements['data'].text]})"
  
  @memory_accesses << ma
end

def canonical(vars)
  result = Array.new
  next_number = 0
  viewed = Hash.new
  vars.each{|var|
    number = viewed[var]
    number = (next_number += 1) if number == nil
    result << number
    viewed.merge!({var => number})
  }
  result
end

def solve template_file
  raise "please add the first argument for xml with test template" if template_file.nil?
  raise "please add the second argument for xml with initial microprocessor state" if ARGV[1].nil?
  template = File.new template_file
  
  raise "file not found: #{template_file}" if template.nil?
  
  @unique_counter = 0
  @data_builder = DataBuilder.new
  @memory_accesses = []
  
  puts "(benchmark tesla"
  puts ":logic QF_BV"
  
  doc = REXML::Document.new template
  
  @l1Hits = []
  @mtlbHits = []
  previous_tagsets = []
  
  @initlength = 10 # TODO сделать n*w+M
  init_tagsets = []
  (1..@initlength).each{|i|
    tagset = "tagset#{@unique_counter += 1}" 
    puts ":extrafuns (( #{tagset} #{@TAGSETTYPE} ))"
    puts ":assumption"
      puts "(and true "
      init_tagsets.each{|t| puts "(= bit0 (bvcomp #{t} #{tagset}))"}
      puts ")"
    init_tagsets << tagset
  }
  
  instructions = Hash.new
  
  doc.elements.each('template/instruction/situation/memory') do |memory|
    cacheTestSituation = memory.elements['cache'].attributes['id']
    microTLBSituation = memory.elements['microtlb'].attributes['id']
      
    # ввести переменную для этой тестовой ситуации и записать ее в SMT-LIB
    tagset = "tagset#{@unique_counter += 1}" 
    puts ":extrafuns (( #{tagset} #{@TAGSETTYPE} ))"
     
    # сделать ограничения для cacheTS >< microTLBS и выдать их на out
    @l1Hits << tagset if cacheTestSituation == "l1Hit"
    @mtlbHits << tagset if microTLBSituation == "mtlbHit"
    puts ":assumption"
    send(cacheTestSituation, init_tagsets, previous_tagsets, tagset)
    puts ":assumption"
    puts send(microTLBSituation, previous_tagsets, tagset)
    previous_tagsets << tagset
    
    instruction = memory.parent.parent
    ins = Instruction.new
    instructions.merge!( {instruction => ins} )
    
    # ввести виртуальный адрес инструкции
    ins.virtual_address = "va#{@unique_counter += 1}"
    ins.phys_after_translation = "pat#{@unique_counter += 1}"
    ins.phys_before_memaccess = "pbma#{@unique_counter += 1}"
    ins.tagset = tagset
    ins.data = "data#{@unique_counter += 1}"
  end

  @synonyms = Hash.new
  @varlengths = Hash.new
  # ввести определения регистров и констант
  doc.elements.each('template/register | template/constant') { |reg|
      puts ":extrafuns (( #{reg.attributes['id']}_X BitVec[#{reg.attributes['length']}] ))"  
      @synonyms[reg.attributes['id']] = "#{reg.attributes['id']}_X"
      @varlengths[reg.attributes['id']] = reg.attributes['length'].to_i
  }
  
  # странслировать определния тестовых ситуаций
  doc.elements.each('template/instruction') {|instruction|
      (instruction.attributes["clone"]||"1").to_i.times{|i|
          process_instruction instruction, instructions[instruction]
      }
  }
 
  puts ")"
  end
end


orig = $stdout
f = File.open('out.smt', 'w')
$stdout = f
Solver.new.solve ARGV[0]
f.close
$stdout = orig

#File.open('out.smt'){|file| puts file.read }

puts `z3 /m out.smt`


#File.delete('out.smt')
