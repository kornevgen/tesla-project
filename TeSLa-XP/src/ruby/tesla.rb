### эта версия корректно работает только со случаями Cached >< Mapped!

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
    
    @nonmicropfns = dataxml.elements.collect("data/tlb/pfn"){ |pfn|
                      pfn.attributes["value"].to_i }
    
    @pfns = @micropfns + @nonmicropfns
    
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

def l1Hit_mtlbHit_part1(previous_tagsets, current_tagset)
    "(and " +
        "(or false" + previous_tagsets.collect { |t|
            " (= #{current_tagset} #{t} )" }.join + ")" +    
            
        "(or false" + previous_tagsets.collect { |t|
            " (= #{getPfn current_tagset} #{getPfn t}) " }.join + ")" +
     ")"
end

def l1Hit_mtlbHit_part2(previous_tagsets, current_tagset)

 "(and " +
    "(or false" +
    @data_builder.LinterPFN.collect{ |lambda,delta|
        "(and " +
      	"(= #{current_tagset} bv#{lambda}[#{@TAGSETLEN}])" +
		cache_tagset_is_not_displaced_yet( previous_tagsets, current_tagset, lambda, delta ) + ")"
	}.join + ")" +

    "(or false" + previous_tagsets.collect { |tagset|
         " (= #{getPfn current_tagset} #{getPfn tagset}) " }.join + ")" +
  ")"
end

def l1Hit_mtlbHit_part3(previous_tagsets, current_tagset)  
  "(and " +
    "(or " +
      @data_builder.M.collect{|m|
            "(and " +
              "(= #{getPfn current_tagset} bv#{m}[#{@PFNLEN}]) " +
		  tlb_pfn_is_not_displaced_yet(previous_tagsets, current_tagset, m) + ")"
          }.join + ")" +
    
    "(or false" + previous_tagsets.collect { |tagset|
          " (= #{current_tagset} #{tagset}) " }.join + ")" +  
  ")"
end

def l1Hit_mtlbHit_part4(previous_tagsets, current_tagset)
  "(or false " +
      @data_builder.LinterM.collect{|lambda,delta|
        "(and (= #{current_tagset} bv#{lambda}[#{@TAGSETLEN}])" +
            cache_tagset_is_not_displaced_yet(previous_tagsets, current_tagset, lambda, delta) +
		tlb_pfn_is_not_displaced_yet(previous_tagsets, current_tagset, lambda/128) +
        ")" }.join + ")"
end

def l1Hit_mtlbHit(previous_tagsets, tagset)
  puts ":assumption"
  puts "(or false "
  (1..4).each{|n| puts send("l1Hit_mtlbHit_part#{n}", previous_tagsets, tagset) }
  puts ")"
end

def l1Hit_mtlbMiss_part1(previous_tagsets, current_tagset)
  "(or false " +
      @data_builder.LinterPFNminusM.collect{|lambda,delta|
        "(and (= #{current_tagset} bv#{lambda}[#{@TAGSETLEN}])" +
              cache_tagset_is_not_displaced_yet(previous_tagsets, current_tagset, lambda, delta) +
        ")" }.join + ")" 
end

def l1Hit_mtlbMiss_part2(previous_tagsets, current_tagset)
  "(or false " +
      @data_builder.LinterM.collect{|lambda,delta|
        "(and (= #{current_tagset} bv#{lambda}[#{@TAGSETLEN}])" +
              cache_tagset_is_not_displaced_yet(previous_tagsets, current_tagset, lambda, delta) +
              tlb_pfn_is_displaced_already(previous_tagsets, current_tagset, lambda/128) +
        ")" }.join + ")" 
end

def cache_tagset_is_not_displaced_yet(previous_tagsets, current_tagset, lambda, delta)
          if delta + 1 > @L1ASSOC - previous_tagsets.length
              "(>= #{@L1ASSOC - delta - 1} (+ 0 " +
                    (0..previous_tagsets.length-1).collect{|i|
                        if @l1Hits.include? previous_tagsets[i]
                            "(and " +
                                "(or " + @data_builder.L.
                                            select{|l,d| l%128 == lambda%128 }.
                                            select{|l,d| d > delta }.
                                            collect{|l,d|
                                         "(= #{previous_tagsets[i]} bv#{l}[#{@TAGSETLEN}]) "
                                    }.join + ")" +
                                
                                previous_tagsets.values_at(0..i-1).collect{|t|
                                  "(/= #{previous_tagsets[i]} #{t} ) "
                                }.join +
                            ")"
                        else
                          "(= #{getRegion previous_tagsets[i]} #{getRegion current_tagset} )"
                        end }.collect{|f| "(ite #{f} 1 0)"}.join +
              "))"
          else
            " true"
          end
end

def cache_tagset_is_displaced_already(previous_tagsets, current_tagset, lambda, delta)
          if delta + 1 > @L1ASSOC - previous_tagsets.length
              "(< #{@L1ASSOC - delta - 1} (+ 0 " +
                    (0..previous_tagsets.length-1).collect{|i|
                        if @l1Hits.include? previous_tagsets[i]
                            "(and " +
                                "(or " + @data_builder.L.
                                            select{|l,d| l%128 == lambda%128 }.
                                            select{|l,d| d > delta }.
                                            collect{|l,d|
                                         "(= #{previous_tagsets[i]} bv#{l}[#{@TAGSETLEN}]) "
                                    }.join + ")" +
                                
                                previous_tagsets.values_at(0..i-1).collect{|t|
                                  "(/= #{previous_tagsets[i]} #{t} ) "
                                }.join +
                            ")"
                        else
                          "(= #{getRegion previous_tagsets[i]} #{getRegion current_tagset} )"
                        end }.collect{|f| "(ite #{f} 1 0)"}.join +                     
              "))"
          else
            " false"
          end
end

def tlb_pfn_is_not_displaced_yet(previous_tagsets, current_tagset, m)
	delta_T = @data_builder.M.index(m)

            if delta_T + 1 > @TLBASSOC - previous_tagsets.length
                "(>= #{@TLBASSOC - delta_T - 1 - previous_tagsets.length + @mtlbHits.length} (+ 0 " +
                      (0..previous_tagsets.length-1).collect{|i| if @mtlbHits.include? previous_tagsets[i]
                      "(and " +
                          "(or " +
                          	@data_builder.M.values_at(delta_T+1..@TLBASSOC-1).collect{|mtail|
	                            "(= #{getPfn previous_tagsets[i]} bv#{mtail}[#{@PFNLEN}]) "
      	                    }.join +
                          ")" +
                          
                          previous_tagsets.values_at(0..i-1).collect{|t|
                            "(= bit0 (bvcomp #{getPfn previous_tagsets[i]} #{getPfn t} )) "
                          }.join +
                      ")"
                    end }.compact.collect{|f| " (ite #{f} 1 0)"}.join +                     
                "))"
            else " true"
            end
end

def tlb_pfn_is_displaced_already(previous_tagsets, current_tagset, m)
	delta_T = @data_builder.M.index(m)
	previous_mtlbmiss_count = previous_tagsets.length - @mtlbHits.length

  if delta_T + 1 > @TLBASSOC - previous_tagsets.length
			if delta_T < @TLBASSOC - previous_mtlbmiss_count #it is right!
                "(< #{@TLBASSOC - delta_T - 1 - previous_mtlbmiss_count} (+ 0 " +
                      (0..previous_tagsets.length-1).collect{|i| if @mtlbHits.include? previous_tagsets[i]
                      "(and " +
                          "(or " +
                          	@data_builder.M.values_at(delta_T+1..@TLBASSOC-1).collect{|mtail|
	                            "(= #{getPfn previous_tagsets[i]} bv#{mtail}[#{@PFNLEN}]) "
      	                    }.join +
                          ")" +
                          
                          previous_tagsets.values_at(0..i-1).collect{|t|
                            "(= bit0 (bvcomp #{getPfn previous_tagsets[i]} #{getPfn t} )) "
                          }.join +
                      ")"
                    end }.compact.collect{|f| " (ite #{f} 1 0)"}.join +                     
                "))"
			else
				" true"
			end
    else " false"
    end
end


def l1Hit_mtlbMiss(previous_tagsets, tagset)
  previous_tagsets.each{|t|
    puts ":assumption"
    puts "(= bit0 (bvcomp #{getRegion t} #{getRegion tagset}))"
  }
  
  puts ":assumption"
  puts "(or false "
  (1..2).each{|n| puts send("l1Hit_mtlbMiss_part#{n}", previous_tagsets, tagset) }
  puts ")"
end

def l1Miss_mtlbHit_part1 previous_tagsets, tagset
  "(and " + 
      @data_builder.LinterM.collect{|lambda,delta|
          " (= bit0 (bvcomp #{tagset} bv#{lambda}[#{@TAGSETLEN}]))"
      }.join +
      
      "(or " + @data_builder.M.collect{|m|
          "(and " +
              "(= #{getPfn tagset} bv#{m}[#{@PFNLEN}])" +
              tlb_pfn_is_not_displaced_yet(previous_tagsets, tagset, m) +
          ")"
      }.join + ")" +
  ")"
end

def l1Miss_mtlbHit_part2 previous_tagsets, tagset
  "(and " +
      @data_builder.LinterPFN.collect{|lambda,delta|
          "(= bit0 (bvcomp #{tagset} bv#{lambda}[#{@TAGSETLEN}]))"
      }.join +
      
      "(or " + previous_tagsets.collect{|prev_tagset|
          "(= bit0 (bvcomp #{getRegion tagset} #{getRegion prev_tagset}))"
      }.join + ")" +
  ")"
end

def l1Miss_mtlbHit_part3 previous_tagsets, tagset
  "(or false " +
      @data_builder.LinterM.collect{|lambda,delta|
          "(and " +
                "(= #{tagset} bv#{lambda}[#{@TAGSETLEN}])" +
                cache_tagset_is_displaced_already(previous_tagsets, tagset, lambda, delta) +
                tlb_pfn_is_not_displaced_yet(previous_tagsets, tagset, lambda/128) +
          ")"
      }.join + ")"
end

def l1Miss_mtlbHit_part4 previous_tagsets, tagset
  "(or false " +
      @data_builder.LinterPFN.collect{|lambda,delta|
          "(and " +
                "(= #{tagset} bv#{lambda}[#{@TAGSETLEN}])" +
                cache_tagset_is_displaced_already(previous_tagsets, tagset, lambda, delta) +
                "(or " + previous_tagsets.collect{|prev_tagset|
                          "(= bit0 (bvcomp #{getRegion tagset} #{getRegion prev_tagset}))"
                          }.join + ")" +
          ")"
      }.join + ")"
end

def l1Miss_mtlbMiss_part1 previous_tagsets, tagset
  "(and true " + 
      @data_builder.LinterPFNminusM.collect{|lambda,delta|
          " (= bit0 (bvcomp #{tagset} bv#{lambda}[#{@TAGSETLEN}]))"
      }.join + ")"
end

def l1Miss_mtlbMiss_part2 previous_tagsets, tagset
  "(or false " +
      @data_builder.LinterPFNminusM.collect{|lambda,delta|
          "(and " +
              "(= #{tagset} bv#{lambda}[#{@TAGSETLEN}])" +
              cache_tagset_is_displaced_already(previous_tagsets, tagset, lambda, delta) + ")"
      }.join + ")"
end

def l1Miss_mtlbMiss_part3 previous_tagsets, tagset
  "(and " +
        @data_builder.LinterM.collect{|lambda,delta|
            "(= bit0 (bvcomp #{tagset} bv#{lambda}[#{@TAGSETLEN}]))"
        }.join +
        
        "(or " +
            @data_builder.M.collect{|m|
                "(and (= #{getPfn tagset} bv#{m}[#{@PFNLEN}]) " +
                      tlb_pfn_is_displaced_already(previous_tagsets, tagset, m) + ")"
            }.join + ")" +
  ")"
end

def l1Miss_mtlbMiss_part4 previous_tagsets, tagset
  "(or false " +
      @data_builder.LinterM.collect{|lambda,delta|
          "(and " +
                "(= #{tagset} bv#{lambda}[#{@TAGSETLEN}])" +
                cache_tagset_is_displaced_already(previous_tagsets, tagset, lambda, delta) +
                tlb_pfn_is_displaced_already(previous_tagsets, tagset, lambda/128) + ")"
      }.join + ")"
end

def l1Miss_mtlbHit( previous_tagsets, tagset )
  previous_tagsets.each{|t|
    puts ":assumption"
    puts "(/= #{t} #{tagset})"
  }
  
  puts ":assumption"
  puts "(or false "
  (1..4).each{|n| puts send("l1Miss_mtlbHit_part#{n}", previous_tagsets, tagset) }
  puts ")"  
end

def l1Miss_mtlbMiss( previous_tagsets, tagset )
  previous_tagsets.each{|t|
    puts ":assumption"
    puts "(= bit0 (bvcomp #{getRegion t} #{getRegion tagset}))"
  }
  
  puts ":assumption"
  puts "(or false "
  (1..4).each{|n| puts send("l1Miss_mtlbMiss_part#{n}", previous_tagsets, tagset) }
  puts ")"
end

def process_instruction(instruction, ins_object)
  path = instruction.attributes['name'] +
  "/" + instruction.elements['situation'].attributes['name'] + ".xml"
  test_situation = REXML::Document.new File.new(path)  

  reverse_synonyms = Hash[*test_situation.get_elements('situation/argument').  ## перевести в текст??
      zip(instruction.get_elements('argument')).flatten]

  # for 'result' test situation arguments change synonyms and define new variables
  test_situation.elements.each('//[@state="result"]'){|arg|
       new_name = "#{@synonyms[reverse_synonyms[arg].text]}_X"
       puts ":extrafuns (( #{new_name} BitVec[#{arg.attributes['length']}] ))"
       @synonyms[reverse_synonyms[arg].text] = "#{@synonyms[reverse_synonyms[arg].text]}_X"
  }
  
  full_context = Hash.new
  @lengths_context = Hash.new
  reverse_synonyms.each{|tsarg, insarg|
      full_context[tsarg.attributes['name']] = @synonyms[insarg.text]
      @lengths_context[tsarg.attributes['name']] = @varlengths[insarg.text]
  }
  full_context.merge! @synonyms
  @lengths_context.merge! @varlengths
  @ins_object = ins_object
  
  # traverse operators
  test_situation.elements.each('situation/*[not(starts-with(name(),"argument"))]'){|operator|
      send( "constraintsfrom_#{operator.name}", operator, full_context )
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
  
  # модель виртуальной памяти
  #TODO можно сделать более полный Cached Mapped сегмент
  puts "(= bv0[33] (extract[63:31] #{@ins_object.virtual_address}))"  

  # соответствие некоторой строке TLB
  pfn_name = "_localvar_#{@unique_counter += 1}"
  vpndiv2_name = "_localvar_#{@unique_counter += 1}"
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
  puts "(= #{@ins_object.phys_after_translation} " + 
    "(concat (extract[#{@TAGSETLEN-1}:#{@TAGSETLEN-@PFNLEN}] #{@ins_object.tagset}) " + 
            "(extract[#{@PABITS-@PFNLEN-1}:0] #{@ins_object.virtual_address}) ) )"
end

def constraintsfrom_LoadMemory( operator, full_context )
  puts ":extrafuns(( #{@ins_object.data} BitVec[64] ))"
  ma = MemoryAccess.new
  ma.dwPhysAddr = "_localvar_#{@unique_counter += 1}"
  ma.type = "LOAD"
  ma.data = @ins_object.data
  puts ":extrafuns(( #{ma.dwPhysAddr} BitVec[#{@PABITS-3}] ))"
  
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
  
  @memory_accesses << ma
end

def solve
  raise "please add the first argument for xml with test template" if ARGV[0].nil?
  raise "please add the second argument for xml with initial microprocessor state" if ARGV[1].nil?
  template = File.new ARGV[0]
  
  raise "file not found: #{ARGV[0]}" if template.nil?
  
  @unique_counter = 0
  @data_builder = DataBuilder.new
  @memory_accesses = []
  
  puts "(benchmark tesla"
  puts ":logic QF_BV"
  
  doc = REXML::Document.new template
  
  @l1Hits = []
  @mtlbHits = []
  previous_tagsets = []
  
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
    send("#{cacheTestSituation}_#{microTLBSituation}", previous_tagsets, tagset)
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
  doc.elements.each('template/register | template/constant') { |reg|
      puts ":extrafuns (( #{reg.attributes['id']}_X BitVec[#{reg.attributes['length']}] ))"  
      @synonyms[reg.attributes['id']] = "#{reg.attributes['id']}_X"
      @varlengths[reg.attributes['id']] = reg.attributes['length'].to_i
  }
  
  doc.elements.each('template/instruction') {|instruction|
      #instruction.attributes["clone"].to_i.times{|i|
      process_instruction instruction, instructions[instruction]
      #}
  }
 
  puts ")"
  end
end

orig = $stdout
f = File.open('out.smt', 'w')
$stdout = f
Solver.new.solve
f.close
$stdout = orig

File.open('out.smt'){|file| puts file.read }

puts `z3 /m out.smt`

File.delete('out.smt')