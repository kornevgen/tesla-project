# TODO эта версия корректно работает только со случаями Cached >< Mapped!

# TODO сделать поддержку нового формата XML-описаний (шаблонов, тестовых ситуаций, состояний)

require "rexml/document"

require "tesla"

class DataBuilder
  
  def initialize(data_file)
    @tagsets = Hash.new
    dataxml = REXML::Document.new File.new(data_file)
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
  
  def PFNminusM
    @nonmicropfns
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

$TAGSETLEN = 31
$PFNLEN = 24
$TAGSETTYPE = "BitVec[#{$TAGSETLEN}]"
$L1ASSOC = 4
$TLBASSOC = 4
$SEGBITS = 40
$PABITS = 36
$MASK = 0

class CombinedSolver < Solver
  
def solve template_file, data_file
    @data_builder = DataBuilder.new data_file
    solve template_file
end

def getPfn(tagset)
  "(extract[30:7] #{tagset})"
end

def getRegion(tagset)
  "(extract[6:0] #{tagset})"
end

def l1Hit_mtlbHit_part1(previous_tagsets, current_tagset)
#    "(and " +
        "(or false" + previous_tagsets.collect { |t|
            " (= #{current_tagset} #{t} )" }.join + ")"   
            
#        "(or false" + previous_tagsets.collect { |t|
#            " (= #{getPfn current_tagset} #{getPfn t}) " }.join + ")" +
#     ")"
end

def l1Hit_mtlbHit_part2(previous_tagsets, current_tagset)

 "(and " +
    "(or false" +
    @data_builder.LinterPFN.collect{ |lambda,delta|
        "(and " +
      	"(= #{current_tagset} bv#{lambda}[#{$TAGSETLEN}])" +
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
              "(= #{getPfn current_tagset} bv#{m}[#{$PFNLEN}]) " +
		           tlb_pfn_is_not_displaced_yet(previous_tagsets, current_tagset, m) + ")"
          }.join + ")" +
    
    "(or false" + previous_tagsets.collect { |tagset|
          " (= #{current_tagset} #{tagset}) " }.join + ")" +  
  ")"
end

def l1Hit_mtlbHit_part4(previous_tagsets, current_tagset)
  "(or false " +
      @data_builder.LinterM.collect{|lambda,delta|
        "(and (= #{current_tagset} bv#{lambda}[#{$TAGSETLEN}])" +
            cache_tagset_is_not_displaced_yet(previous_tagsets, current_tagset, lambda, delta) +
		        tlb_pfn_is_not_displaced_yet(previous_tagsets, current_tagset, lambda/128) +
        ")" }.join + ")"
end

def l1Hit_mtlbHit(previous_tagsets, tagset)
  puts ":assumption"
  puts "(or "
  (1..4).each{|n| puts send("l1Hit_mtlbHit_part#{n}", previous_tagsets, tagset) }
  puts ")"
end

def l1Hit_mtlbMiss_part1(previous_tagsets, current_tagset)
  "(or false " +
      @data_builder.LinterPFNminusM.collect{|lambda,delta|
        "(and (= #{current_tagset} bv#{lambda}[#{$TAGSETLEN}])" +
              cache_tagset_is_not_displaced_yet(previous_tagsets, current_tagset, lambda, delta) +
        ")" }.join + ")" 
end

def l1Hit_mtlbMiss_part2(previous_tagsets, current_tagset)
  "(or false " +
      @data_builder.LinterM.collect{|lambda,delta|
        "(and (= #{current_tagset} bv#{lambda}[#{$TAGSETLEN}])" +
              cache_tagset_is_not_displaced_yet(previous_tagsets, current_tagset, lambda, delta) +
              tlb_pfn_is_displaced_already(previous_tagsets, current_tagset, lambda/128) +
        ")" }.join + ")" 
end

def cache_cardinality_constraint( lambda, delta, previous_tagsets, current_tagset, relation, sumlength)
    "(#{relation} bv#{$L1ASSOC - delta - 1}[#{sumlength}] (bvadd bv0[#{sumlength}] " +
          (0..previous_tagsets.length-1).collect{|i|
              if @l1Hits.include? previous_tagsets[i]
                  "(and " +
                      "(or " + @data_builder.L.\
                                  select{|l,d| l%128 == lambda%128 }.
                                  select{|l,d| d > delta }.
                                  collect{|l,d|
                               "(= #{previous_tagsets[i]} bv#{l}[#{$TAGSETLEN}]) "
                          }.join + ")" +
                      
                      previous_tagsets[0..i-1].collect{|t|
                        "(= bit0 (bvcomp #{previous_tagsets[i]} #{t} )) " }.join +
                  ")"
              else
                "(= #{getRegion previous_tagsets[i]} #{getRegion current_tagset} )"
              end }.collect{|f| "(ite #{f} bv1[#{sumlength}] bv0[#{sumlength}])"}.join +
    "))"
end

def cache_tagset_is_not_displaced_yet(previous_tagsets, current_tagset, lambda, delta)
  if $L1ASSOC - delta - previous_tagsets.length > 0
    " true "
  else
      cache_cardinality_constraint( lambda, delta, previous_tagsets, current_tagset, "bvuge",\
        (Math.log(@l1Hits.length + 1) / Math.log(2)).ceil )
  end
end

def cache_tagset_is_displaced_already(previous_tagsets, current_tagset, lambda, delta)
  if $L1ASSOC - delta - previous_tagsets.length > 0
    " false "
  else
      cache_cardinality_constraint( lambda, delta, previous_tagsets, current_tagset, "bvult",\
        (Math.log(@l1Hits.length + 1) / Math.log(2)).ceil )
  end
end

def tlb_cardinality_constraint(delta_T, previous_tagsets, relation, sumlength)
      "(#{relation} bv#{$TLBASSOC - delta_T - previous_tagsets.length + @mtlbHits.length}[#{sumlength}] (bvadd bv0[#{sumlength}] " +
            (0..previous_tagsets.length-1).collect{|i| if @mtlbHits.include? previous_tagsets[i]
            "(and " +
                "(or " +
                  @data_builder.M[delta_T..$TLBASSOC-1].collect{|mtail|
                    "(= #{getPfn previous_tagsets[i]} bv#{mtail}[#{$PFNLEN}]) " }.join +
                ")" +
                
                previous_tagsets[0..i-1].collect{|t|
                  "(= bit0 (bvcomp #{getPfn previous_tagsets[i]} #{getPfn t} )) " }.join +
            ")"
          end }.compact.collect{|f| " (ite #{f} bv1[#{sumlength}] bv0[#{sumlength}])"}.join +                     
      "))"  
end

def tlb_pfn_is_not_displaced_yet(previous_tagsets, current_tagset, m)
  delta_T = @data_builder.M.index(m) + 1
  
  if $TLBASSOC - delta_T - previous_tagsets.length > 0
    " true "
  elsif $TLBASSOC - delta_T - (previous_tagsets.length - @mtlbHits.length) <= 0
    " false "
  else
    tlb_cardinality_constraint(delta_T, previous_tagsets, "bvuge",\
      (Math.log(@mtlbHits.length + 1) / Math.log(2)).ceil )
  end
end

def tlb_pfn_is_displaced_already(previous_tagsets, current_tagset, m)
  delta_T = @data_builder.M.index(m) + 1
  mtlbmissc = previous_tagsets.length - @mtlbHits.length
  
  if $TLBASSOC - delta_T - mtlbmissc <= -1
    " true "
  elsif $TLBASSOC - delta_T - previous_tagsets.length >= 0
    " false "
  else
    tlb_cardinality_constraint(delta_T, previous_tagsets, "bvult",\
      (Math.log(@mtlbHits.length + 1) / Math.log(2)).ceil  )
  end
end


def l1Hit_mtlbMiss(previous_tagsets, tagset)
  previous_tagsets.each{|t|
    puts ":assumption"
    puts "(= bit0 (bvcomp #{getRegion t} #{getRegion tagset}))"
  }
  
  puts ":assumption"
  puts "(or "
  (1..2).each{|n| puts send("l1Hit_mtlbMiss_part#{n}", previous_tagsets, tagset) }
  puts ")"
end

def l1Miss_mtlbHit_part1 previous_tagsets, tagset
  "(and " + 
      @data_builder.LinterM.collect{|lambda,delta|
          " (= bit0 (bvcomp #{tagset} bv#{lambda}[#{$TAGSETLEN}]))"
      }.join +
      
      "(or " + @data_builder.M.collect{|m|
          "(and " +
              "(= #{getPfn tagset} bv#{m}[#{$PFNLEN}])" +
              tlb_pfn_is_not_displaced_yet(previous_tagsets, tagset, m) +
          ")"
      }.join + ")" +
  ")"
end

def l1Miss_mtlbHit_part2 previous_tagsets, tagset
  "(and " +
      @data_builder.LinterPFN.collect{|lambda,delta|
          "(= bit0 (bvcomp #{tagset} bv#{lambda}[#{$TAGSETLEN}]))"
      }.join +
      
      "(or false " + previous_tagsets.collect{|prev_tagset|
          "(= #{getRegion tagset} #{getRegion prev_tagset})"
      }.join + ")" +
  ")"
end

def l1Miss_mtlbHit_part3 previous_tagsets, tagset
  "(or false " +
      @data_builder.LinterM.collect{|lambda,delta|
          "(and " +
                "(= #{tagset} bv#{lambda}[#{$TAGSETLEN}])" +
                cache_tagset_is_displaced_already(previous_tagsets, tagset, lambda, delta) +
                tlb_pfn_is_not_displaced_yet(previous_tagsets, tagset, lambda/128) +
          ")"
      }.join + ")"
end

def l1Miss_mtlbHit_part4 previous_tagsets, tagset
  "(or false " +
      @data_builder.LinterPFN.collect{|lambda,delta|
          "(and " +
                "(= #{tagset} bv#{lambda}[#{$TAGSETLEN}])" +
                cache_tagset_is_displaced_already(previous_tagsets, tagset, lambda, delta) +
                "(or " + previous_tagsets.collect{|prev_tagset|
                          "(= #{getRegion tagset} #{getRegion prev_tagset})"
                          }.join + ")" +
          ")"
      }.join + ")"
end

def l1Miss_mtlbMiss_part1 previous_tagsets, tagset
  "(and true " + 
      @data_builder.LinterPFNminusM.collect{|lambda,delta|
          " (= bit0 (bvcomp #{tagset} bv#{lambda}[#{$TAGSETLEN}]))"
      }.join + ")"
end

def l1Miss_mtlbMiss_part2 previous_tagsets, tagset
  "(or false " +
      @data_builder.LinterPFNminusM.collect{|lambda,delta|
          "(and " +
              "(= #{tagset} bv#{lambda}[#{$TAGSETLEN}])" +
              cache_tagset_is_displaced_already(previous_tagsets, tagset, lambda, delta) + ")"
      }.join + ")"
end

def l1Miss_mtlbMiss_part3 previous_tagsets, tagset
  "(and " +
        @data_builder.LinterM.collect{|lambda,delta|
            "(= bit0 (bvcomp #{tagset} bv#{lambda}[#{$TAGSETLEN}]))"
        }.join +
        
        "(or " +
            @data_builder.M.collect{|m|
                "(and (= #{getPfn tagset} bv#{m}[#{$PFNLEN}]) " +
                      tlb_pfn_is_displaced_already(previous_tagsets, tagset, m) + ")"
            }.join + ")" +
  ")"
end

def l1Miss_mtlbMiss_part4 previous_tagsets, tagset
  "(or false " +
      @data_builder.LinterM.collect{|lambda,delta|
          "(and " +
                "(= #{tagset} bv#{lambda}[#{$TAGSETLEN}])" +
                cache_tagset_is_displaced_already(previous_tagsets, tagset, lambda, delta) +
                tlb_pfn_is_displaced_already(previous_tagsets, tagset, lambda/128) + ")"
      }.join + ")"
end

def l1Miss_mtlbHit( previous_tagsets, tagset )
  previous_tagsets.each{|t|
    puts ":assumption"
    puts "(= bit0 (bvcomp #{t} #{tagset}))"
  }
  
  puts ":assumption"
  puts "(or "
  (1..4).each{|n| puts send("l1Miss_mtlbHit_part#{n}", previous_tagsets, tagset) }
  puts ")"  
end

def l1Miss_mtlbMiss( previous_tagsets, tagset )
  previous_tagsets.each{|t|
    puts ":assumption"
    puts "(= bit0 (bvcomp #{getRegion t} #{getRegion tagset}))"
  }
  
  puts ":assumption"
  puts "(or "
  (1..4).each{|n| puts send("l1Miss_mtlbMiss_part#{n}", previous_tagsets, tagset) }
  puts ")"
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
  ins_object = @instructions_objects[@instruction]
  
  ins_object = @instructions_objects[@instruction]
  new_name = "_localvar_#{@unique_counter += 1}"
  full_context[operator.attributes['name']] = new_name
  
  @lengths_context[operator.attributes['name']] = 64
  puts ":extrafuns (( #{new_name} BitVec[64] ))"
  puts ":extrafuns (( #{ins_object.data} BitVec[64] ))"
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


def constraintsfrom_AddressTranslation( operator, full_context )
  ins_object = @instructions_objects[@instruction]
  
  @lengths_context.merge!({operator.elements['physical'].text => $PABITS})
  
  # get physical var from instructions repository
  physical = ins_object.phys_after_translation
  full_context[operator.elements['physical'].text] = physical
    
  puts ":extrafuns(( #{ins_object.virtual_address} BitVec[64] ))"
  puts ":extrafuns(( #{ins_object.phys_after_translation} BitVec[#{$PABITS}] ))"
  
  puts ":assumption"
  puts "(= #{ins_object.virtual_address} #{full_context[operator.elements['virtual'].text]})"
  
  puts ":assumption"
  puts "(= #{ins_object.phys_after_translation} #{full_context[operator.elements['physical'].text]})"
  
  # модель виртуальной памяти
  #TODO можно сделать более полный Cached Mapped сегмент
  puts ":assumption"
  puts "(= bv0[33] (extract[63:31] #{ins_object.virtual_address}))"  

  # соответствие некоторой строке TLB
  pfn_name = "_localvar_#{@unique_counter += 1}"
  vpndiv2_name = "_localvar_#{@unique_counter += 1}"
  puts ":assumption"
  puts "(let (#{pfn_name} #{getPfn( ins_object.tagset )})"
  puts "(let (#{vpndiv2_name} (extract[#{$SEGBITS-1}:#{$PABITS-$PFNLEN}] #{ins_object.virtual_address}))"
  
  puts "(or "
  @data_builder.TLB.select{|l| l.mask == $MASK && l.r == 0}.each{|tlbline|
    if tlbline.CCA0 == "Cached"
      puts "(and (= #{pfn_name} bv#{tlbline.pfn0}[#{$PFNLEN}])" +
          "(= #{vpndiv2_name} bv#{tlbline.vpndiv2 * 2}[#{$SEGBITS-$PABITS+$PFNLEN}]))"
    end
    if tlbline.CCA1 == "Cached"
      puts "(and (= #{pfn_name} bv#{tlbline.pfn1}[#{$PFNLEN}])" +
          "(= #{vpndiv2_name} bv#{tlbline.vpndiv2 * 2 + 1}[#{$SEGBITS-$PABITS+$PFNLEN}]))"
    end
  }
  puts")))"
    
  # определение physical_after_translation
  puts ":assumption"
  puts "(= #{ins_object.phys_after_translation} " + 
    "(concat (extract[#{$TAGSETLEN-1}:#{$TAGSETLEN-$PFNLEN}] #{ins_object.tagset}) " + 
            "(extract[#{$PABITS-$PFNLEN-1}:0] #{ins_object.virtual_address}) ) )"
            
  # TODO (это только частный случай) часть виртуального адреса -- это часть тегсета (сет)
  puts ":assumption"
  puts "(= (extract[11:5] #{ins_object.virtual_address}) (extract[6:0] #{ins_object.tagset}))"
end

def constraintsfrom_LoadMemory( operator, full_context )
  ins_object = @instructions_objects[@instruction]
  
  @lengths_context[operator.elements[1].text] = 64
  
  data = ins_object.data
  full_context[operator.elements[1].text] = data
  
  puts ":extrafuns(( #{ins_object.data} BitVec[64] ))"
  ma = MemoryAccess.new
  ma.dwPhysAddr = "_localvar_#{@unique_counter += 1}"
  ma.type = "LOAD"
  ma.data = ins_object.data
  puts ":extrafuns(( #{ma.dwPhysAddr} BitVec[#{$PABITS-3}] ))"
  
  puts ":assumption"
  puts "(= #{ins_object.data} #{full_context[operator.elements['data'].text]})"
  
  puts ":assumption"
  puts "(and true "
  @memory_accesses.reverse.each{|maccess|
      if maccess.type == "LOAD"
          puts "(implies (= #{maccess.dwPhysAddr} #{ma.dwPhysAddr}) (= #{ins_object.data} #{maccess.data}))"
      else
          puts "(ite (= #{maccess.dwPhysAddr} #{ma.dwPhysAddr}) (= #{ins_object.data} #{maccess.data}) (and true "
      end
  }
  @memory_accesses.select{|m| m.type=="STORE"}.each{|m| puts "))" }
  puts ")"
  
  @memory_accesses << ma
end

def constraintsfrom_StoreMemory( operator, full_context )
  ins_object = @instructions_objects[@instruction]
  
  ma = MemoryAccess.new
  ma.dwPhysAddr = "_localvar_#{@unique_counter += 1}"
  ma.type = "STORE"
  ma.data = ins_object.data
  puts ":extrafuns(( #{ma.dwPhysAddr} BitVec[#{$PABITS-3}] ))"
  
  puts ":assumption"
  puts "(= #{ins_object.data} #{full_context[operator.elements['data'].text]})"
  
  @memory_accesses << ma
end

def procedures_preparations
  @l1Hits = []
  @mtlbHits = []
  previous_tagsets = []
  
  @instructions_objects = Hash.new
  
  #совместная генерация не дает возможности разделить этот цикл на части по процедурам
  doc.elements.each('template/instruction/situation/memory'){ |memory|
    cacheTestSituation = memory.elements['cache'].attributes['id']
    microTLBSituation = memory.elements['microtlb'].attributes['id']
      
    # ввести переменную для этой тестовой ситуации и записать ее в SMT-LIB
    tagset = "tagset#{@unique_counter += 1}" 
    puts ":extrafuns (( #{tagset} #{$TAGSETTYPE} ))"
     
    # сделать ограничения для cacheTS >< microTLBS и выдать их на out
    @l1Hits << tagset if cacheTestSituation == "l1Hit"
    @mtlbHits << tagset if microTLBSituation == "mtlbHit"
    send("#{cacheTestSituation}_#{microTLBSituation}", previous_tagsets, tagset)
    previous_tagsets << tagset
    
    instruction = memory.parent.parent
    ins = Instruction.new
    @instructions_objects.merge!( {instruction => ins} )
    
    # ввести виртуальный адрес инструкции
    ins.virtual_address = "va#{@unique_counter += 1}"
    ins.phys_after_translation = "pat#{@unique_counter += 1}"
    ins.phys_before_memaccess = "pbma#{@unique_counter += 1}"
    ins.tagset = tagset
    ins.data = "data#{@unique_counter += 1}"
  }
end

end


class MirrorSolver < Solver

def solve template_file, data_file
    @data_builder = DataBuilder.new data_file
    solve template_file
end

def getPfn(tagset)
  "(extract[30:7] #{tagset})"
end

def getRegion(tagset)
  "(extract[6:0] #{tagset})"
end

def l1Hit( init_tagsets, previous_tagsets, current_tagset )
  mirror init_tagsets, previous_tagsets, current_tagset, ">",\
      (Math.log([$L1ASSOC, init_tagsets.length + previous_tagsets.length].max + 1)/Math.log(2)).ceil
end

def l1Miss( init_tagsets, previous_tagsets, current_tagset )
  mirror init_tagsets, previous_tagsets, current_tagset, "<=",\
      (Math.log([$L1ASSOC, init_tagsets.length + previous_tagsets.length].max + 1)/Math.log(2)).ceil
end

def mirror( init_tagsets, previous_tagsets, current_tagset, mirrrelation, sumlength )
  puts "(and "
    puts "(or "
          (init_tagsets + previous_tagsets).each{|t|
              puts "(= #{t} #{current_tagset})"  }
    puts ")"
    
    puts "(#{mirrrelation} #{$L1ASSOC} (+ "
          
          # u(t_i)
          (0.. init_tagsets.length-1).each{|i|
            puts "(ite (and "
               init_tagsets[i..init_tagsets.length-1].each{|t|
                      puts "(= bit0 (bvcomp #{t} #{current_tagset}))" } 
               previous_tagsets.each{|t|
                      puts "(= bit0 (bvcomp #{t} #{current_tagset}))" }
               puts "(= #{getRegion init_tagsets[i]} #{getRegion current_tagset}))" +
            " 1 0 ) " }
            
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
                
            puts " ) 1 0 ) " }
          
      puts " )))"
end

def mtlbHit(previous_tagsets, current_tagset)  
    "(or " +
      @data_builder.M.collect{|m|
            "(and " +
              "(= #{getPfn current_tagset} bv#{m}[#{$PFNLEN}]) " +
               tlb_pfn_is_not_displaced_yet(previous_tagsets, current_tagset, m) + ")"
          }.join + 
    
      previous_tagsets.collect { |tagset|
          " (= #{getPfn current_tagset} #{getPfn tagset}) " }.join + ")"
end

def mtlbMiss(previous_tagsets, current_tagset)  
  "(and " +
    "(or " +
      @data_builder.PFNminusM.collect{|m|
        "(= #{getPfn current_tagset} bv#{m}[#{$PFNLEN}]) "
      }.join +
      @data_builder.M.collect{|m|
            "(and " +
              "(= #{getPfn current_tagset} bv#{m}[#{$PFNLEN}]) " +
               tlb_pfn_is_displaced_already(previous_tagsets, current_tagset, m) + ")"
          }.join + 
    ")" +    
     previous_tagsets.collect { |tagset|
          " (= bit0 (bvcomp #{getPfn current_tagset} #{getPfn tagset})) " }.join + ")"
end

def tlb_cardinality_constraint(delta_T, previous_tagsets, relation, sumlength)
      "(#{relation} #{$TLBASSOC - delta_T - previous_tagsets.length + @mtlbHits.length} (+ 0 " +
            (0..previous_tagsets.length-1).collect{|i| if @mtlbHits.include? previous_tagsets[i]
            "(and " +
                "(or " +
                  @data_builder.M[delta_T..$TLBASSOC-1].collect{|mtail|
                    "(= #{getPfn previous_tagsets[i]} bv#{mtail}[#{$PFNLEN}]) " }.join +
                ")" +
                
                previous_tagsets[0..i-1].collect{|t|
                  "(= bit0 (bvcomp #{getPfn previous_tagsets[i]} #{getPfn t} )) " }.join +
            ")"
          end }.compact.collect{|f| " (ite #{f} 1 0)"}.join +                     
      "))"  
end

def tlb_pfn_is_not_displaced_yet(previous_tagsets, current_tagset, m)
  delta_T = @data_builder.M.index(m) + 1
  
  if $TLBASSOC - delta_T - previous_tagsets.length > 0
    " true "
  elsif $TLBASSOC - delta_T - (previous_tagsets.length - @mtlbHits.length) <= 0
    " false "
  else
    tlb_cardinality_constraint(delta_T, previous_tagsets, ">=",\
      (Math.log(@mtlbHits.length + 1) / Math.log(2)).ceil )
  end
end

def tlb_pfn_is_displaced_already(previous_tagsets, current_tagset, m)
  delta_T = @data_builder.M.index(m) + 1
  mtlbmissc = previous_tagsets.length - @mtlbHits.length
  
  if $TLBASSOC - delta_T - mtlbmissc <= -1
    " true "
  elsif $TLBASSOC - delta_T - previous_tagsets.length >= 0
    " false "
  else
    tlb_cardinality_constraint(delta_T, previous_tagsets, "<",\
      (Math.log(@mtlbHits.length + 1) / Math.log(2)).ceil  )
  end
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
  ins_object = @instructions_objects[@instruction]
  
  new_name = "_localvar_#{@unique_counter += 1}"
  full_context[operator.attributes['name']] = new_name
  
  @lengths_context[operator.attributes['name']] = 64
  puts ":extrafuns (( #{new_name} BitVec[64] ))"
  puts ":extrafuns (( #{ins_object.data} BitVec[64] ))"
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

def constraintsfrom_AddressTranslation( operator, full_context )
  ins_object = @instructions_objects[@instruction]
  
  puts ":extrafuns(( #{ins_object.virtual_address} BitVec[64] ))"
  puts ":extrafuns(( #{ins_object.phys_after_translation} BitVec[#{$PABITS}] ))"
  
  puts ":assumption"
  puts "(= #{ins_object.virtual_address} #{full_context[operator.elements['virtual'].text]})"
  
  puts ":assumption"
  puts "(= #{ins_object.phys_after_translation} #{full_context[operator.elements['physical'].text]})"
  
  # модель виртуальной памяти
  #TODO можно сделать более полный Cached Mapped сегмент
  puts ":assumption"
  puts "(= bv0[33] (extract[63:31] #{ins_object.virtual_address}))"  

  # соответствие некоторой строке TLB
  pfn_name = "_localvar_#{@unique_counter += 1}"
  vpndiv2_name = "_localvar_#{@unique_counter += 1}"
  puts ":assumption"
  puts "(let (#{pfn_name} #{getPfn( ins_object.tagset )})"
  puts "(let (#{vpndiv2_name} (extract[#{$SEGBITS-1}:#{$PABITS-$PFNLEN}] #{ins_object.virtual_address}))"
  
  puts "(or "
  @data_builder.TLB.select{|l| l.mask == $MASK && l.r == 0}.each{|tlbline|
    if tlbline.CCA0 == "Cached"
      puts "(and (= #{pfn_name} bv#{tlbline.pfn0}[#{$PFNLEN}])" +
          "(= #{vpndiv2_name} bv#{tlbline.vpndiv2 * 2}[#{$SEGBITS-$PABITS+$PFNLEN}]))"
    end
    if tlbline.CCA1 == "Cached"
      puts "(and (= #{pfn_name} bv#{tlbline.pfn1}[#{$PFNLEN}])" +
          "(= #{vpndiv2_name} bv#{tlbline.vpndiv2 * 2 + 1}[#{$SEGBITS-$PABITS+$PFNLEN}]))"
    end
  }
  puts")))"
    
  # определение physical_after_translation
  puts ":assumption"
  puts "(= #{ins_object.phys_after_translation} " + 
    "(concat (extract[#{$TAGSETLEN-1}:#{$TAGSETLEN-$PFNLEN}] #{ins_object.tagset}) " + 
            "(extract[#{$PABITS-$PFNLEN-1}:0] #{ins_object.virtual_address}) ) )"
            
  # TODO (это только частный случай) часть виртуального адреса -- это часть тегсета (сет)
  puts ":assumption"
  puts "(= (extract[11:5] #{ins_object.virtual_address}) (extract[6:0] #{ins_object.tagset}))"
end

def constraintsfrom_LoadMemory( operator, full_context )
  ins_object = @instructions_objects[@instruction]
  
  puts ":extrafuns(( #{ins_object.data} BitVec[64] ))"
  ma = MemoryAccess.new
  ma.dwPhysAddr = "_localvar_#{@unique_counter += 1}"
  ma.type = "LOAD"
  ma.data = ins_object.data
  puts ":extrafuns(( #{ma.dwPhysAddr} BitVec[#{$PABITS-3}] ))"
  
  puts ":assumption"
  puts "(= #{ins_object.data} #{full_context[operator.elements['data'].text]})"
  
  puts ":assumption"
  puts "(and true "
  @memory_accesses.reverse.each{|maccess|
      if maccess.type == "LOAD"
          puts "(implies (= #{maccess.dwPhysAddr} #{ma.dwPhysAddr}) (= #{ins_object.data} #{maccess.data}))"
      else
          puts "(ite (= #{maccess.dwPhysAddr} #{ma.dwPhysAddr}) (= #{ins_object.data} #{maccess.data}) (and true "
      end
  }
  @memory_accesses.select{|m| m.type=="STORE"}.each{|m| puts "))" }
  puts ")"
  
  @memory_accesses << ma
end

def constraintsfrom_StoreMemory( operator, full_context )
  ins_object = @instructions_objects[@instruction]
  
  ma = MemoryAccess.new
  ma.dwPhysAddr = "_localvar_#{@unique_counter += 1}"
  ma.type = "STORE"
  ma.data = ins_object.data
  puts ":extrafuns(( #{ma.dwPhysAddr} BitVec[#{$PABITS-3}] ))"
  
  puts ":assumption"
  puts "(= #{ins_object.data} #{full_context[operator.elements['data'].text]})"
  
  @memory_accesses << ma
end



def procedures_preparations
  @l1Hits = []
  @mtlbHits = []
  previous_tagsets = []
  
  init_tagsets = []
  (1..$initlength).each{|i|
    tagset = "tagset#{@unique_counter += 1}" 
    puts ":extrafuns (( #{tagset} #{$TAGSETTYPE} ))"
    puts ":assumption"
      puts "(and true "
      init_tagsets.each{|t| puts "(= bit0 (bvcomp #{t} #{tagset}))"}
      puts ")"
    init_tagsets << tagset
  }
  
  @instructions_objects = Hash.new 
  doc.elements.each('template/instruction/situation/memory'){ |memory|
    cacheTestSituation = memory.elements['cache'].attributes['id']
    microTLBSituation = memory.elements['microtlb'].attributes['id']
      
    # ввести переменную для этой тестовой ситуации и записать ее в SMT-LIB
    tagset = "tagset#{@unique_counter += 1}" 
    puts ":extrafuns (( #{tagset} #{$TAGSETTYPE} ))"
     
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
    @instructions_objects.merge!( {instruction => ins} )
    
    # ввести виртуальный адрес инструкции
    ins.virtual_address = "va#{@unique_counter += 1}"
    ins.phys_after_translation = "pat#{@unique_counter += 1}"
    ins.phys_before_memaccess = "pbma#{@unique_counter += 1}"
    ins.tagset = tagset
    ins.data = "data#{@unique_counter += 1}"
  }
end

end
