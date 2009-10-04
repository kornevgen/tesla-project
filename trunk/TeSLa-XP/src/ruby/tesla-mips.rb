# TODO эта версия корректно работает только со случаями Cached >< Mapped!

# TODO по-другому задается тестовая ситуация: через access, а не memory

#TODO при построении ограничений учтено, что TLB не меняется... 
#(т.е. тут только TLBHit, но возможны mtlbHit и mtlbMiss)

require "rexml/document"

require "tesla"

class Instruction
  attr_accessor :tagset
  attr_accessor :virtual_address
  attr_accessor :vpnd2
end

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
  
  def getVPNdiv2(m)
      @tlb.select{|tlbline| tlbline.pfn0 == m || tlbline.pfn1 == m }.first.vpndiv2
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
$VPNdiv2LEN = $SEGBITS - $PABITS + $PFNLEN
$VPNd2TYPE = "BitVec[#{$VPNdiv2LEN}]"

=begin
  класс содержит процедуры для MIPS, но не содержит способа генерации ограничений
  для последовательности тегсетов
=end
class MIPS_Solver < Solver
  
def solve2 template_file, data_file
    @data_builder = DataBuilder.new data_file
    solve template_file
end

def getPfn(tagset)
  "(extract[30:7] #{tagset})"
end

def getRegion(tagset)
  "(extract[6:0] #{tagset})"
end

def getVPNdiv2(virtual_address)
  "(extract[#{$SEGBITS-1}:#{$PABITS-$PFNLEN}] #{virtual_address})"
end

def getOddBit(virtual_address)
  "(extract[#{$PABITS - $PFNLEN}:#{$PABITS - $PFNLEN}] #{virtual_address})"
end
  
def let_block(var, node, full_context, &body)
    puts "(let (#{var} " + send("constraintsfrom_#{node.name}", node, full_context) + ")" 
    yield
    puts ")"
end

def def_localvar(operator, full_context, id, required_length = 0)
  var = "_localvar_#{@unique_counter += 1}"
  node = operator.elements["argument[@id='#{id}']"][0]  
  
  bitlen = send("length_#{node.name}", node )
  if required_length > 0 && bitlen != required_length
    raise "Bit length of #{id} must be #{required_length}"
  end

  puts ":extrafuns ((#{var} BitVec[#{bitlen}]))"
  puts ":assumption"
  puts "(= #{var} " + send("constraintsfrom_#{node.name}", node, full_context) + ")" 

  var
end

def constraintsfrom_BytesSelect( operator, full_context )
  selected = full_context[operator.elements['argument[@id="selected"]/new'].text]
  index = "_localvar_#{@unique_counter += 1}"
  index_node = operator.elements['argument[@id="index"]'][0]
  content = "_localvar_#{@unique_counter += 1}"
  content_node = operator.elements['argument[@id="content"]'][0]
  type = operator.elements['argument[@id="type"]'][0]
  
  puts ":assumption"
  if type == "WORD"
    let_block(index, index_node, full_context ){
    let_block(content, content_node, full_context ){
          puts     "(ite (= bv0[3] #{index}) " + 
                      "(= #{selected} (extract[31:0] #{content}))" +
                      "(= #{selected} (extract[63:32] #{content}))" + 
                   ")" }}
  elsif type == "DOUBLEWORD"
    let_block(content, content_node, full_context ){
          puts "(= #{selected} #{content})" }
  #TODO elsif остальные типы
  else
    raise "Unknown bytes-select type '#{type}}'"
  end
end

def constraintsfrom_BytesExpand( operator, full_context )  
  expanded = full_context[operator.elements['argument[@id="expanded"]/new'].text]
  type = operator.elements['argument[@id="type"]'][0]
  index = "_localvar_#{@unique_counter += 1}"
  index_node = operator.elements['argument[@id="index"]'][0]
  content = "_localvar_#{@unique_counter += 1}"
  content_node = operator.elements['argument[@id="content"]'][0]
  
  puts ":assumption"
  if type == "BYTE"
    let_block(index, index_node, full_context ){
    let_block(content, content_node, full_context ){
          (0..7).each{|number|
              puts "(ite (= #{index} bv#{number}[3])" +
                "(= #{expanded} (concat (concat bv0[#{56-8*number}] (extract[7:0] #{content})) bv0[#{8*number}]))"
          }
          puts "false"
          puts (1..8).collect{")"}.join }}
  elsif type == "DOUBLEWORD"
    let_block(content, content_node, full_context ){
          puts "(= #{expanded} #{content})" }
  #TODO elsif сделать остальные типы
  else
    raise "unknown bytes-select type '#{type}'"
  end
end

def constraintsfrom_AddressTranslation( operator, full_context )
  ins_object = @instruction_objects[@instruction]
  tagset = ins_object.tagset
  
  virtual_address = def_localvar(operator, full_context, "virtual", 64)
  puts ":assumption"
  puts "(= #{virtual_address} #{ins_object.virtual_address})"
  
  raise "Bit length of physical address must be #{$PABITS}" \
    if operator.elements['argument[@id="physical"]/new'].attributes['length'].to_i != $PABITS
  
  # модель виртуальной памяти
  #TODO можно сделать более полный Cached Mapped сегмент
  puts ":assumption"
  puts "(= bv0[33] (extract[63:31] #{virtual_address}))"  

#  # соответствие некоторой строке TLB
#  pfn_name = "_localvar_#{@unique_counter += 1}"
#  vpndiv2_name = "_localvar_#{@unique_counter += 1}"
#  puts ":assumption"
#  puts "(let (#{pfn_name} #{getPfn( tagset )})"
#  puts "(let (#{vpndiv2_name} (extract[#{$SEGBITS-1}:#{$PABITS-$PFNLEN}] #{virtual_address}))"
#  
#  puts "(or "
#  @data_builder.TLB.select{|l| l.mask == $MASK && l.r == 0}.each{|tlbline|
#    if tlbline.CCA0 == "Cached"
#      puts "(and (= #{pfn_name} bv#{tlbline.pfn0}[#{$PFNLEN}])" +
#          "(= #{vpndiv2_name} bv#{tlbline.vpndiv2 * 2}[#{$SEGBITS-$PABITS+$PFNLEN}]))"
#    end
#    if tlbline.CCA1 == "Cached"
#      puts "(and (= #{pfn_name} bv#{tlbline.pfn1}[#{$PFNLEN}])" +
#          "(= #{vpndiv2_name} bv#{tlbline.vpndiv2 * 2 + 1}[#{$SEGBITS-$PABITS+$PFNLEN}]))"
#    end
#  }
#  puts")))"
    
  # определение physical_after_translation
  puts ":assumption"
  puts "(= " + full_context[operator.elements['argument[@id="physical"]/new'].text] + 
    "(concat (extract[#{$TAGSETLEN-1}:#{$TAGSETLEN-$PFNLEN}] #{tagset}) " + 
            "(extract[#{$PABITS-$PFNLEN-1}:0] #{virtual_address}) ) )"
            
  # часть виртуального адреса -- это часть тегсета (сет)
  puts ":assumption"
  puts "(= (extract[#{$PABITS-$PFNLEN-1}:#{$PABITS-$TAGSETLEN}] #{virtual_address}) " + 
          "(extract[#{$TAGSETLEN - $PFNLEN - 1}:0] #{tagset})  )"
end

def constraintsfrom_LoadMemory( operator, full_context )
  data = full_context[operator.elements['argument[@id="data"]/new'].text]  
  physical_address = def_localvar(operator, full_context, "physical", $PABITS)
  
  ma = MemoryAccess.new
  ma.dwPhysAddr = "_localvar_#{@unique_counter += 1}"
  ma.type = "LOAD"
  ma.data = data
  puts ":extrafuns(( #{ma.dwPhysAddr} BitVec[#{$PABITS-3}] ))"
  puts ":assumption"
  puts "(= #{ma.dwPhysAddr} (extract[#{$PABITS-1}:3] #{physical_address}) )"
  
# это ограничение нужно, если физический адрес после трансляции адреса меняется:
#  puts ":assumption"
#  puts "(= #{@instruction_objects[@instruction].tagset} " +
#      " (extract[#{$PABITS-1}:#{$PABITS-$TAGSETLEN}] #{physical_address}))" 
  
  puts ":assumption"
  puts "(and true "
  @memory_accesses.reverse.each{|maccess|
      if maccess.type == "LOAD"
          puts "(implies (= #{maccess.dwPhysAddr} #{ma.dwPhysAddr}) (= #{data} #{maccess.data}))"
      else
          puts "(ite (= #{maccess.dwPhysAddr} #{ma.dwPhysAddr}) (= #{data} #{maccess.data}) (and true "
      end
  }
  @memory_accesses.select{|m| m.type=="STORE"}.each{|m| puts "))" }
  puts ")"
  
  @memory_accesses << ma
end

def constraintsfrom_StoreMemory( operator, full_context )
  physical_address = def_localvar(operator, full_context, "physical", $PABITS)
  data = def_localvar(operator, full_context, "data", 64)
  
  ma = MemoryAccess.new
  ma.dwPhysAddr = "_localvar_#{@unique_counter += 1}"
  ma.type = "STORE"
  ma.data = data
  puts ":extrafuns(( #{ma.dwPhysAddr} BitVec[#{$PABITS-3}] ))"
  puts ":assumption"
  puts "(= #{ma.dwPhysAddr} (extract[#{$PABITS-1}:3] #{physical_address}))"

  
# это ограничение нужно, если физический адрес после трансляции адреса меняется:
#  puts ":assumption"
#  puts "(= #{@instruction_objects[@instruction].tagset} " + 
#  " (extract[#{$PABITS-1}:#{$PABITS-$TAGSETLEN}] #{physical_address}))" 
  
  @memory_accesses << ma
end
end


class MIPS_CombinedSolver < MIPS_Solver
  
def vpn_pfn(tagset, virtual_address)
    # соответствие некоторой строке TLB
  pfn_name = "_pfn#{@unique_counter += 1}"
  vpndiv2_name = "_vpndiv2#{@unique_counter += 1}"
  puts ":assumption"
  puts "(let (#{pfn_name} #{getPfn( tagset )})"
  puts "(let (#{vpndiv2_name} (extract[#{$SEGBITS-1}:#{$PABITS-$PFNLEN}] #{virtual_address}))"
  
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

def l1Hit_mtlbHit(previous_tagsets, tagset, virtual_address)
  puts ":assumption"
  puts "(or "
  (1..4).each{|n| puts send("l1Hit_mtlbHit_part#{n}", previous_tagsets, tagset) }
  puts ")"
  
  vpn_pfn(tagset, virtual_address)
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
                      
                      (0..i-1).collect{|i1| 
                        "(= bit0 (bvcomp #{previous_tagsets[i]} #{previous_tagsets[i1]} )) " }.join +
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
                
                (0..i-1).collect{|i1|
                  "(= bit0 (bvcomp #{getPfn previous_tagsets[i]} #{getPfn previous_tagsets[i1]} )) " }.join +
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


def l1Hit_mtlbMiss(previous_tagsets, tagset, virtual_address)
  previous_tagsets.each{|t|
    puts ":assumption"
    puts "(= bit0 (bvcomp #{getRegion t} #{getRegion tagset}))"
  }
  
  puts ":assumption"
  puts "(or "
  (1..2).each{|n| puts send("l1Hit_mtlbMiss_part#{n}", previous_tagsets, tagset) }
  puts ")"
  
  vpn_pfn(tagset, virtual_address)
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

def l1Miss_mtlbHit( previous_tagsets, tagset, virtual_address )
  previous_tagsets.each{|t|
    puts ":assumption"
    puts "(= bit0 (bvcomp #{t} #{tagset}))"
  }
  
  puts ":assumption"
  puts "(or "
  (1..4).each{|n| puts send("l1Miss_mtlbHit_part#{n}", previous_tagsets, tagset) }
  puts ")" 
  
  vpn_pfn(tagset, virtual_address)
end

def l1Miss_mtlbMiss( previous_tagsets, tagset, virtual_address )
  previous_tagsets.each{|t|
    puts ":assumption"
    puts "(= bit0 (bvcomp #{getRegion t} #{getRegion tagset}))"
  }
  
  puts ":assumption"
  puts "(or "
  (1..4).each{|n| puts send("l1Miss_mtlbMiss_part#{n}", previous_tagsets, tagset) }
  puts ")"
  
  vpn_pfn(tagset, virtual_address)
end


def procedures_preparations doc
  @l1Hits = []
  @mtlbHits = []
  previous_tagsets = []
  
  @tagsets = Hash.new
  
  #совместная генерация не дает возможности разделить этот цикл на части по процедурам
  doc.elements.each('template/instruction/situation/memory'){ |memory|
    cacheTestSituation = memory.elements['cache'].attributes['id']
    microTLBSituation = memory.elements['microtlb'].attributes['id']
      
    # ввести переменную для этой тестовой ситуации и записать ее в SMT-LIB
    tagset = "_ts#{@unique_counter += 1}"
    virtual_address = "_va#{@unique_counter += 1}"
    puts ":extrafuns (( #{tagset} #{$TAGSETTYPE} ) ( #{virtual_address} BitVec[64]))"
     
    # сделать ограничения для cacheTS >< microTLBS и выдать их на out
    @l1Hits << tagset if cacheTestSituation == "l1Hit"
    @mtlbHits << tagset if microTLBSituation == "mtlbHit"
    send("#{cacheTestSituation}_#{microTLBSituation}", previous_tagsets, tagset, virtual_address)
    previous_tagsets << tagset
    
    ins_object = Instruction.new
    ins_object.tagset = tagset
    ins_object.virtual_address = virtual_address
    @instruction_objects.merge!( {memory.parent.parent => ins_object} )
  }
end

end


class MIPS_FullMirrorSolver < MIPS_Solver

def l1Hit( init_tagsets, previous_tagsets, current_tagset )
  mirror_l1 init_tagsets, previous_tagsets, current_tagset, ">",\
      (Math.log([$L1ASSOC, init_tagsets.length + previous_tagsets.length].max + 1)/Math.log(2)).ceil
end

def l1Miss( init_tagsets, previous_tagsets, current_tagset )
  mirror_l1 init_tagsets, previous_tagsets, current_tagset, "<=",\
      (Math.log([$L1ASSOC, init_tagsets.length + previous_tagsets.length].max + 1)/Math.log(2)).ceil
end

def mtlbHit( init_vpnd2s, previous_vpnd2s, current_vpnd2 )
  mirror_mtlb init_vpnd2s.last($TLBASSOC-1), previous_vpnd2s, current_vpnd2, ">"
end

def mtlbMiss( init_vpnd2s, previous_vpnd2s, current_vpnd2 )
  puts "(or" # or нужен только лишь в том случае, если длина init > TLBASSOC-1
  (0..init_vpnd2s.length - $TLBASSOC).each{|i|
      puts "(= #{current_vpnd2} #{init_vpnd2s[i]})" }
  mirror_mtlb(init_vpnd2s.last($TLBASSOC-1), previous_vpnd2s, current_vpnd2, "<=")
  puts ")"
end

def mirror_l1( init_tagsets, previous_tagsets, current_tagset, mirrrelation, sumlength )
  puts "(and "
    puts "(or "
          (init_tagsets + previous_tagsets).each{|t|
              puts "(= #{t} #{current_tagset})"  }
    puts ")"
    
    puts "(#{mirrrelation} #{$L1ASSOC} (+ 0 "
          
          # u(t_i)
          if ! previous_tagsets.empty?
            puts "(ite (or "
            previous_tagsets.each{|t|
                  puts "(= #{t} #{current_tagset})" }
            puts ") 0 (+ 0 "
            
            (0.. init_tagsets.length-1).each{|i|
              puts "(ite (and true (or false "
                 (0..i-1).each{|i1|
                        puts "(= #{init_tagsets[i1]} #{current_tagset})" } 
                 puts ")"
                 puts "(= #{getRegion init_tagsets[i]} #{getRegion current_tagset}))" +
              " 1 0 ) " }
            puts"))"
          end
            
          # u(x_i): S_i = miss/hit
          (0.. previous_tagsets.length-1).each{|i|
            puts "(ite (and true "
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
                    (0..i-1).each{|i1|
                        puts "(= #{current_tagset} #{previous_tagsets[i1]})" }
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


def mirror_mtlb( init_vpnd2s, previous_vpnd2s, current_vpnd2, mirrrelation )
  puts "(and "
    puts "(or "
          (init_vpnd2s + previous_vpnd2s).each{|t|
              puts "(= #{t} #{current_vpnd2})"  }
    puts ")"
    
    puts "(#{mirrrelation} #{$TLBASSOC} (+ 0 "
          
          # u(t_i)
          if ! previous_vpnd2s.empty?
            puts "(ite (or "
               previous_vpnd2s.each{|t|
                     puts "(= #{t} #{current_vpnd2})" }
               puts ") 0 (+ 0 "
            (0.. init_vpnd2s.length-1).each{|i|
              puts "(ite (or false "
                 (0..i-1).each{|i1|
                       puts "(= #{init_vpnd2s[i1]} #{current_vpnd2})" }
              puts ") 1 0)"
             }
             puts"))"
         end
            
          # u(x_i): S_i = miss/hit
          (0.. previous_vpnd2s.length-1).each{|i|
            puts "(ite (and true "
               previous_vpnd2s[i..previous_vpnd2s.length-1].each{|t|
                      puts "(= bit0 (bvcomp #{t} #{current_vpnd2}))"
               }
               
               if @mtlbHits.include?( previous_vpnd2s[i] )
                (0..init_vpnd2s.length-1).each{|j|
                  puts "(or "
                    # c(t_j) = 0
                    init_vpnd2s[j..init_vpnd2s.length-1].each{|t|
                        puts "(= #{current_vpnd2} #{t})" }
                    (0..i-1).each{|i1|
                        puts "(= #{current_vpnd2} #{previous_vpnd2s[i1]})" }
                    puts "(= bit0 (bvcomp #{previous_vpnd2s[i]} #{init_vpnd2s[j]})))"
                }
                (0..i-1).each{|j|
                  puts "(or "
                    # c(t_j) = 0
                    previous_vpnd2s[j..i-1].each{|t|
                        puts "(= #{current_vpnd2} #{t})" }
                    puts "(= bit0 (bvcomp #{previous_vpnd2s[i]} #{previous_vpnd2s[j]})))"
                }
               end
                
            puts " ) 1 0 ) " }
          
      puts " )))"
end

def to_h(keys, values)
  Hash[*([keys, values].transpose.flatten)]
end

def procedures_preparations doc
  raise "Please define initlength" if $initlength.nil? || $initlength == 0
  raise "Please define initlength_mtlb" if $initlength_mtlb.nil? || $initlength_mtlb == 0
  
  @l1Hits = []
  @mtlbHits = []
  previous_tagsets = []
  previous_objects = []
  previous_vpnd2s = []
  
  init_tagsets = Array.new($initlength){"_its#{@unique_counter += 1}"}
  init_tagsets.each{|t| puts ":extrafuns (( #{t} #{$TAGSETTYPE} ))" }
  puts ":assumption"
  puts "(distinct #{init_tagsets.join(' ')})"
#  init_tagsets.inject([]){|ts,t|
#    ts.each{|t1| puts ":assumption";puts"(= bit0 (bvcomp #{t} #{t1}))"}
#    ts + [t]
#  }
  
  init_vpnd2s = Array.new($initlength_mtlb){"_ivpnd#{@unique_counter += 1}"}
  init_vpnd2s.each{|t| puts ":extrafuns (( #{t} #{$VPNd2TYPE} ))" }
#  puts ":assumption"
#  puts "(distinct #{init_vpnd2s.join(' ')})"
  init_vpnd2s.inject([]){|ts,t|
    ts.each{|t1| puts ":assumption";puts"(= bit0 (bvcomp #{t} #{t1}))"}
    ts + [t]
  }
  
  @instruction_objects = Hash.new 
  doc.elements.each('template/instruction/situation/memory'){ |memory|
    cacheTestSituation = memory.elements['cache'].attributes['id']
    microTLBSituation = memory.elements['microtlb'].attributes['id']
      
    # ввести переменную для этой тестовой ситуации и записать ее в SMT-LIB
    tagset = "_ts#{@unique_counter += 1}"
    vpnd2 = "_vpnd#{@unique_counter += 1}"
    virtual_address = "_va#{@unique_counter += 1}"
    puts ":extrafuns (( #{tagset} #{$TAGSETTYPE} ) " + 
         "(#{virtual_address} BitVec[64])" +
         "(#{vpnd2} #{$VPNd2TYPE}))"
    
    # vpnd2 is bits of virtual_address
    puts ":assumption"
    puts "(= #{getVPNdiv2 virtual_address} #{vpnd2})"
    
    # разных vpn'в не более количества строк TLB, но для маленьких экспериментов это всегда верно

    #каждый init_vpnd2 не может быть одновременно использован с разными oddbit
    #если совпадают vpn, то совпадают pfn
    previous_objects.each{|o|
        puts ":assumption"
        puts "(implies (= #{vpnd2} #{o.vpnd2}) " +
        "(ite (= #{getOddBit virtual_address} #{getOddBit o.virtual_address} ) "
        puts "(= #{o.tagset} #{tagset}) " +
          init_vpnd2s.inject("(and true "){|s, v| s+"(= bit0 (bvcomp #{v} #{vpnd2}))" } + " )" +
        "))"
    }
    
    # сделать ограничения для cacheTS >< microTLBS и выдать их на out
    @l1Hits << tagset if cacheTestSituation == "l1Hit"
    @mtlbHits << vpnd2 if microTLBSituation == "mtlbHit"
    puts ":assumption"
    send(cacheTestSituation, init_tagsets, previous_tagsets, tagset)
    puts ":assumption"
    send(microTLBSituation, init_vpnd2s, previous_vpnd2s, vpnd2)
    previous_tagsets << tagset
    previous_vpnd2s << vpnd2
    
    instruction_object = Instruction.new
    instruction_object.tagset = tagset
    instruction_object.vpnd2 = vpnd2
    instruction_object.virtual_address = virtual_address
    @instruction_objects.merge!( {memory.parent.parent => instruction_object} )
    previous_objects << instruction_object
  }
end

end


class MIPS_MirrorSolver < MIPS_Solver

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
            puts "(ite (and true "
               init_tagsets[i..init_tagsets.length-1].each{|t|
                      puts "(= bit0 (bvcomp #{t} #{current_tagset}))" } 
               previous_tagsets.each{|t|
                      puts "(= bit0 (bvcomp #{t} #{current_tagset}))" }
               puts "(= #{getRegion init_tagsets[i]} #{getRegion current_tagset}))" +
            " 1 0 ) " }
            
          # u(x_i): S_i = miss/hit
          (0.. previous_tagsets.length-1).each{|i|
            puts "(ite (and true "
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
                    (0..i-1).each{|i1|
                        puts "(= #{current_tagset} #{previous_tagsets[i1]})" }
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

def mtlbHit(previous_tagsets, current_tagset, virtual_address)  
    "(or false " +
      @data_builder.M.delete_if{|m|
        delta_T = @data_builder.M.index(m) + 1
        $TLBASSOC - delta_T - (previous_tagsets.length - @mtlbHits.length) <= 0
      }.collect{|m|
            "(and " +
              "(= #{getPfn current_tagset} bv#{m}[#{$PFNLEN}]) " +
              "(= #{getVPNdiv2(virtual_address)} bv#{@data_builder.getVPNdiv2(m)}[#{$VPNdiv2LEN}] ) " +
               tlb_pfn_is_not_displaced_yet(previous_tagsets, current_tagset, m) + ")"
          }.join + 
    
      previous_tagsets.collect { |tagset|
          " (= #{getPfn current_tagset} #{getPfn tagset}) " }.join + ")"
end

def mtlbMiss(previous_tagsets, current_tagset, virtual_address)  
  "(and " +
    "(or " +
      @data_builder.PFNminusM.collect{|m|
        "(and " +
          "(= #{getPfn current_tagset} bv#{m}[#{$PFNLEN}]) " +
          "(= #{getVPNdiv2(virtual_address)} bv#{@data_builder.getVPNdiv2(m)}[#{$VPNdiv2LEN}] ) " +
        ")"
      }.join +
      @data_builder.M.delete_if{|m|
        delta_T = @data_builder.M.index(m) + 1
        $TLBASSOC - delta_T - previous_tagsets.length >= 0
      }.collect{|m|
            "(and " +
              "(= #{getPfn current_tagset} bv#{m}[#{$PFNLEN}]) " +
              "(= #{getVPNdiv2(virtual_address)} bv#{@data_builder.getVPNdiv2(m)}[#{$VPNdiv2LEN}] ) " +
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
                
                (0..i-1).collect{|i1|
                "(= bit0 (bvcomp #{getPfn previous_tagsets[i]} #{getPfn previous_tagsets[i1]} )) " }.join +
            ")"
          end }.compact.collect{|f| " (ite #{f} 1 0)"}.join +                     
      "))"  
end

def tlb_pfn_is_not_displaced_yet(previous_tagsets, current_tagset, m)
  delta_T = @data_builder.M.index(m) + 1
  
  if $TLBASSOC - delta_T - previous_tagsets.length > 0
    "" #(and X true) === (and X "")
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
    " "
  elsif $TLBASSOC - delta_T - previous_tagsets.length >= 0
    " false "
  else
    tlb_cardinality_constraint(delta_T, previous_tagsets, "<",\
      (Math.log(@mtlbHits.length + 1) / Math.log(2)).ceil  )
  end
end


def procedures_preparations doc
  @l1Hits = []
  @mtlbHits = []
  previous_tagsets = []
  
  init_tagsets = Array.new($initlength){"_its#{@unique_counter += 1}"}
  init_tagsets.each{|t| puts ":extrafuns (( #{t} #{$TAGSETTYPE} ))" }
  puts ":assumption"
  puts "(distinct #{init_tagsets.join(' ')})"
  
  @instruction_objects = Hash.new 
  doc.elements.each('template/instruction/situation/memory'){ |memory|
    cacheTestSituation = memory.elements['cache'].attributes['id']
    microTLBSituation = memory.elements['microtlb'].attributes['id']
      
    # ввести переменную для этой тестовой ситуации и записать ее в SMT-LIB
    tagset = "_ts#{@unique_counter += 1}"
    virtual_address = "_va#{@unique_counter += 1}"
    puts ":extrafuns (( #{tagset} #{$TAGSETTYPE} ) (#{virtual_address} BitVec[64]))"
     
    # сделать ограничения для cacheTS >< microTLBS и выдать их на out
    @l1Hits << tagset if cacheTestSituation == "l1Hit"
    @mtlbHits << tagset if microTLBSituation == "mtlbHit"
    puts ":assumption"
    send(cacheTestSituation, init_tagsets, previous_tagsets, tagset)
    puts ":assumption"
    puts send(microTLBSituation, previous_tagsets, tagset, virtual_address)
    previous_tagsets << tagset
    
    instruction_object = Instruction.new
    instruction_object.tagset = tagset
    instruction_object.virtual_address = virtual_address
    @instruction_objects.merge!( {memory.parent.parent => instruction_object} )
  }
end

end

