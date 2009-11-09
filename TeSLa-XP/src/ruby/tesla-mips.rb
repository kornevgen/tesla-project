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
  
  def pfn
    "(extract[30:7] #{tagset})"
  end
  
  def vpn
  "(extract[#{$SEGBITS-1}:#{$PABITS-$PFNLEN}] #{virtual_address})"
  end
end

class DomainElement
  attr_accessor :tagset
  attr_accessor :vpnd2
end


class DataBuilder
  
  def initialize(data)
    @tagsets = Hash.new
    dataxml = REXML::Document.new data #File.new(data_file)
    dataxml.elements.each("data/cache/set"){ |set|
        set.elements.each_with_index{ |tag,delta|
              tagvalue = tag.attributes["value"]
              @tagsets[tagvalue.to_i * 2**$SETLEN + set.attributes["value"].to_i ] = delta }
    }
              
    @tagsets2 = dataxml.elements.collect("data/cache/set"){ |set|
        set.elements.collect{ |tag| 
            tag.attributes["value"].to_i * 2**$SETLEN + set.attributes["value"].to_i }
    }
    
    @micropfns = dataxml.elements.collect("data/microtlb/line"){ |line|
            [ line.attributes["pfn0"].to_i, line.attributes["pfn1"].to_i ] }.flatten
    
    @nonmicropfns = dataxml.elements.collect("data/tlb/line"){ |line|
            [ line.attributes["pfn0"].to_i, line.attributes["pfn1"].to_i ] }.flatten
    
    @pfns = @micropfns + @nonmicropfns
    
    @microvpns = dataxml.elements.collect("data/microtlb/line"){ |line|
            line.attributes["vpndiv2"].to_i }
    
    @nonmicrovpns = dataxml.elements.collect("data/tlb/line"){ |line|
            line.attributes["vpndiv2"].to_i }
    
    @vpns = @microvpns + @nonmicrovpns
    
    @microtlb = dataxml.elements.collect("data/microtlb/line") {|line|
        tlbline = TLBLine.new
        tlbline.r = line.attributes["range"].to_i
        tlbline.vpndiv2 = line.attributes["vpndiv2"].to_i
        tlbline.mask = line.attributes["mask"].to_i
        tlbline.pfn0 = line.attributes["pfn0"].to_i
        tlbline.CCA0 = line.attributes["CCA0"]
        tlbline.pfn1 = line.attributes["pfn1"].to_i
        tlbline.CCA1 = line.attributes["CCA1"]
        tlbline
    }
    
    @nonmicrotlb = dataxml.elements.collect("data/tlb/line") {|line|
        tlbline = TLBLine.new
        tlbline.r = line.attributes["range"].to_i
        tlbline.vpndiv2 = line.attributes["vpndiv2"].to_i
        tlbline.mask = line.attributes["mask"].to_i
        tlbline.pfn0 = line.attributes["pfn0"].to_i
        tlbline.CCA0 = line.attributes["CCA0"]
        tlbline.pfn1 = line.attributes["pfn1"].to_i
        tlbline.CCA1 = line.attributes["CCA1"]
        tlbline
    }
    
    @tlb = @microtlb + @nonmicrotlb
  end
  
  def LinterPFN
    @LinterPFN ||= @tagsets.select{|tagset,delta|
                    @pfns.include?(tagset/2**($TAGSETLEN - $PFNLEN)) }
  end
  
  def L
    @tagsets
  end
  
  def M
     @pfns
  end
  
  def LinterM
     @LinterM ||= @tagsets.select{|tagset,delta|
                    @micropfns.include?(tagset/2**($TAGSETLEN - $PFNLEN)) }
  end
  
  def LinterPFNminusM
     @LinterPFNminusM ||= @tagsets.select{|tagset,delta|
                    @nonmicropfns.include?(tagset/2**($TAGSETLEN - $PFNLEN)) }
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
  
  def getL1Position(lambda)
    @tagsets2.select{|s| s.include?(lambda) }.first.index(lambda) + 1
  end
  
  def getL1Tail(lambda)
    s = @tagsets2.select{|ss| ss.include?(lambda) }.first.reverse
    s.first(s.index(lambda))
  end
  
  def getMTLBPosition(lambda)
    @microvpns.index( lambda ) + 1
  end
  
  def getMTLBTail(lambda)
    @microvpns.reverse.first(@microvpns.reverse.index(lambda))
  end
  
  def V0f(previous_instructions)
     @microvpns.select{|v| getMTLBPosition(v) > $TLBASSOC - previous_instructions.length  }.uniq
  end
  
  def V0
     @microvpns
  end
  
  def notV0
     @nonmicrovpns
  end

  def intersection(tlblines)
      @tagsets2.flatten.collect{|ts|
        tlblines.select{|l| [l.pfn0, l.pfn1].include?( ts/2**$SETLEN )}.
            collect{|l| d = DomainElement.new; d.tagset = ts; d.vpnd2 = l.vpndiv2; d} }.flatten
  end
  
  def L1interMTLBf(previous_instructions)
    (intersection @microtlb).select{|d| getMTLBPosition(d.vpnd2) > $TLBASSOC - previous_instructions.length  }.uniq
  end

  def L1interMTLB
    (intersection @microtlb).uniq
  end

  def L1interNotMTLB  
    (intersection @nonmicrotlb).uniq
  end
  
  def L1interTLB
    (L1interMTLB() + L1interNotMTLB()).uniq
  end
  
  def L1interTLBf(previous_instructions)
    (L1interMTLB(previous_instructions) + L1interNotMTLB()).uniq
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

params = [$TAGSETLEN, $PFNLEN, $TAGLEN, $L1ASSOC, $TLBASSOC, $SEGBITS, $PABITS, $MASK]
raise "Not all parameters are assigned (#{params.compact.length} #{params.length} )" if params.compact.length != params.length

#$TAGSETLEN = 31
#$PFNLEN = 24
#$TAGLEN = $PFNLEN
#$L1ASSOC = 4
#$TLBASSOC = 4
#$SEGBITS = 40
#$PABITS = 36
#$MASK = 0
$SETLEN = $TAGSETLEN - $PFNLEN
$TAGSETTYPE = "BitVec[#{$TAGSETLEN}]"
$VPNdiv2LEN = $SEGBITS - $PABITS + $PFNLEN - 1
$VPNd2TYPE = "BitVec[#{$VPNdiv2LEN}]"

class Array
  def distinct
      self.each{|t| self.first(self.index(t)).each{|t1|
        puts ":assumption";puts"(= bit0 (bvcomp #{t} #{t1}))" }}
  end
  
  def isin(t)
    puts isin_s(t)
  end
  
  def isin_s(t)
    if self.empty?
      " false "
    elsif self.length == 1
      "(= #{t} #{self.at(0)})"
    else  
      "(or " + self.collect{|t1| "(= #{t} #{t1})" }.join + ")"
    end
  end
  
  def notisin_s(t)
    if self.empty?
      " "
    elsif self.length == 1
      "(= bit0 (bvcomp #{t} #{self.at(0)}))"
    else  
      "(and " + self.collect{|t1| "(= bit0 (bvcomp #{t} #{t1}))"}.join + ")"
    end
  end
  
  def notisin(t)
    puts notisin_s(t)
  end
end

class String
  def isin(t)
    t.isin self
  end
  
  def isin_s(t)
    t.isin_s self
  end
  
  def notisin_s(t)
    t.notisin_s self
  end
  
  def notisin(t)
    t.notisin self
  end
  
  def pfn
    "(extract[30:7] #{self})"
  end

  def region
    "(extract[6:0] #{self})"
  end
end

=begin
  класс содержит процедуры для MIPS, но не содержит способа генерации ограничений
  для последовательности тегсетов
=end
class MIPS_Solver < Solver
  
def solve2 template, data
    @data_builder = DataBuilder.new data
    solve template
end

def getPfn(tagset)
  tagset.pfn
end

def getRegion(tagset)
  tagset.region
end

def getVPNdiv2(virtual_address)
  "(extract[#{$SEGBITS-1}:#{$PABITS-$PFNLEN+1}] #{virtual_address})"
end

def getVPN(virtual_address)
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

  def l1_useful( lambda, previous_instructions, current_instruction, relation )
          puts "(= bv#{lambda}[#{$TAGSETLEN}] #{current_instruction.tagset}) "
          puts "(#{relation} #{$L1ASSOC - @data_builder.getL1Position(lambda)} (+ 0 "
              previous_instructions.collect{|i|
                  if @l1Hits.include?(i)
                    "(and " +
                      @data_builder.getL1Tail(lambda).collect{|t| "bv#{t}[#{$TAGSETLEN}]"}.isin_s(i.tagset) +
                      previous_instructions.first(previous_instructions.index(i)).collect{|ii| ii.tagset}.notisin_s(i.tagset) +
                    ")"
                  else
                    "(= #{getRegion i.tagset} #{getRegion current_instruction.tagset})"
                  end
              }.each{|f| puts "(ite #{f} 1 0)"}
          puts "))"
  end

  def mtlb_useful( lambda, previous_instructions, current_instruction, relation )
        puts "(= bv#{lambda}[#{$VPNdiv2LEN}] #{current_instruction.vpnd2}) "
        if relation == ">=" && @data_builder.getMTLBPosition(lambda) > $TLBASSOC - previous_instructions.length + @mtlbHits.length \
          || relation == "<" && @data_builder.getMTLBPosition(lambda) <= $TLBASSOC - previous_instructions.length
          puts "false"
        elsif relation == ">=" && @data_builder.getMTLBPosition(lambda) <= $TLBASSOC - previous_instructions.length \
          || relation == "<" && @data_builder.getMTLBPosition(lambda) > $TLBASSOC - previous_instructions.length + @mtlbHits.length
          #nothing
        else
          puts "(#{relation} #{$TLBASSOC - @data_builder.getMTLBPosition(lambda) - previous_instructions.length + @mtlbHits.length} (+ 0 "
              @mtlbHits.collect{|i|
                    "(and " +
                      @data_builder.getMTLBTail(lambda).collect{|t| "bv#{t}[#{$VPNdiv2LEN}]"}.isin_s(i.vpnd2) +
                      previous_instructions.first(previous_instructions.index(i)).collect{|ii| ii.vpnd2}.notisin_s(i.vpnd2) +
                    ")"
              }.each{|f| puts "(ite #{f} 1 0)"}
          puts "))"
        end
  end


end


class MIPS_Combined2Solver < MIPS_Solver
  
def vpn_pfn(tagset, virtual_address)
    # соответствие некоторой строке TLB
  pfn_name = "_pfn#{@unique_counter += 1}"
  vpn_name = "_vpn#{@unique_counter += 1}"
  puts ":assumption"
  puts "(let (#{pfn_name} #{getPfn( tagset )})"
  puts "(let (#{vpn_name} #{getVPN( virtual_address )}))"
  
  puts "(or "
  @data_builder.TLB.select{|l| l.mask == $MASK && l.r == 0}.each{|tlbline|
    if tlbline.CCA0 == "Cached"
      puts "(and (= #{pfn_name} bv#{tlbline.pfn0}[#{$PFNLEN}])" +
          "(= #{vpn_name} bv#{tlbline.vpndiv2 * 2}[#{$SEGBITS-$PABITS+$PFNLEN}]))"
    end
    if tlbline.CCA1 == "Cached"
      puts "(and (= #{pfn_name} bv#{tlbline.pfn1}[#{$PFNLEN}])" +
          "(= #{vpn_name} bv#{tlbline.vpndiv2 * 2 + 1}[#{$SEGBITS-$PABITS+$PFNLEN}]))"
    end
  }
  puts")))"
end

def l1Hit_mtlbHit_part1(previous_tagsets, current_tagset)
  previous_tagsets.isin_s(current_tagset)
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
  puts ":assumption"
  previous_tagsets.collect{|t| getRegion(t) }.not_isin( getRegion(tagset) )
  
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
  puts ":assumption"
  previous_tagsets.not_isin(tagset)
  
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
  
  @instruction_objects = Hash.new
  
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

class MIPS_CombinedSolver < MIPS_Solver
  def vpn_pfn( instruction )
    # соответствие некоторой строке TLB
    pfn_name = "_pfn#{@unique_counter += 1}"
    vpn_name = "_vpn#{@unique_counter += 1}"
    puts ":assumption"
    puts "(let (#{pfn_name} #{instruction.pfn})"
    puts "(let (#{vpn_name} #{instruction.vpn})"
    
    puts "(or "
    @data_builder.TLB.select{|l| l.mask == $MASK && l.r == 0}.each{|tlbline|
      if tlbline.CCA0 == "Cached"
        puts "(and (= #{pfn_name} bv#{tlbline.pfn0}[#{$PFNLEN}])" +
            "(= #{instruction.vpnd2} bv#{tlbline.vpndiv2}[#{$VPNdiv2LEN}])" +
            "(= #{vpn_name} bv#{tlbline.vpndiv2 * 2}[#{$VPNdiv2LEN + 1}]))"
      end
      if tlbline.CCA1 == "Cached"
        puts "(and (= #{pfn_name} bv#{tlbline.pfn1}[#{$PFNLEN}])" +
            "(= #{instruction.vpnd2} bv#{tlbline.vpndiv2}[#{$VPNdiv2LEN}])" +
            "(= #{vpn_name} bv#{tlbline.vpndiv2 * 2 + 1}[#{$VPNdiv2LEN + 1}]))"
      end
    }
    puts")))"
  end


  def l1Hit_mtlbHit_part1(previous_instructions, current_instruction)
    if ! previous_instructions.empty?
      puts "(and "
        previous_instructions.collect{|i| i.tagset}.isin( current_instruction.tagset )
        puts "(or false "
          previous_instructions.collect{|i| i.vpnd2}.isin( current_instruction.vpnd2 )
          @data_builder.V0().
          each{|v| puts "(and true "; mtlb_useful( v, previous_instructions, current_instruction, ">=" ); puts ")" }
        puts ")"
      puts ")"
    end
  end
  
  def l1Hit_mtlbHit_part2(previous_instructions, current_instruction)
    if ! previous_instructions.empty?
      puts "(and "
        previous_instructions.collect{|i| i.vpnd2}.isin( current_instruction.vpnd2 )
        puts "(or false "
          @data_builder.L1interTLB.each{|d|
              puts "(and true";l1_useful( d.tagset, previous_instructions, current_instruction, ">=" );puts")" }
        puts ")"
      puts ")"
    end
  end
  
  def l1Hit_mtlbHit_part3(previous_instructions, current_instruction)
    puts "(or false "
      @data_builder.L1interMTLB.each{|d|
        puts "(and true "
        l1_useful( d.tagset, previous_instructions, current_instruction, ">=" )
        mtlb_useful( d.vpnd2, previous_instructions, current_instruction, ">=" )
        puts ")"}        
    puts ")"
  end
  
  def l1Hit_mtlbHit(previous_instructions, current_instruction)
    puts "(or "
     (1..3).each{|p| send("l1Hit_mtlbHit_part#{p}", previous_instructions, current_instruction)}
    puts ")"
  end
  
  def l1Hit_mtlbMiss(previous_instructions, current_instruction)
    puts "(and "
      previous_instructions.collect{|i| i.vpnd2}.notisin(current_instruction.vpnd2)      
    puts "(or "
     (1..3).each{|p| send("l1Hit_mtlbMiss_part#{p}", previous_instructions, current_instruction)}
    puts "))"
  end
  
  def l1Hit_mtlbMiss_part1(previous_instructions, current_instruction)
    if ! previous_instructions.empty?
      puts "(and "
        previous_instructions.collect{|i| i.tagset}.isin( current_instruction.tagset )
        puts "(or false "
          @data_builder.V0.each{|v| puts"(and true";mtlb_useful( v, previous_instructions, current_instruction, "<" );puts")" }
          @data_builder.notV0.collect{|v| "bv#{v}[#{$VPNdiv2LEN}]"}.isin( current_instruction.vpnd2 )
        puts ")"
      puts ")"
    end
  end
  
  def l1Hit_mtlbMiss_part2(previous_instructions, current_instruction)
      puts "(or false "
        @data_builder.L1interNotMTLB.each{|d|
          puts "(and (= #{current_instruction.vpnd2} bv#{d.vpnd2}[#{$VPNdiv2LEN}]) "
            l1_useful( d.tagset, previous_instructions, current_instruction, ">=" )
          puts ")" }
      puts ")"
  end
  
  def l1Hit_mtlbMiss_part3(previous_instructions, current_instruction)
    puts "(or false "
      @data_builder.L1interMTLB.each{|d|
        puts "(and true"
        l1_useful( d.tagset, previous_instructions, current_instruction, ">=" )
        mtlb_useful( d.vpnd2, previous_instructions, current_instruction, "<" )
        puts ")"}
    puts ")"
  end
  
  def l1Miss_mtlbHit(previous_instructions, current_instruction)
    puts "(and "
      previous_instructions.collect{|i| i.tagset}.notisin(current_instruction.tagset)      
      puts "(or ";(1..3).each{|p| send("l1Miss_mtlbHit_part#{p}", previous_instructions, current_instruction)};puts ")"
    puts ")"
  end
  
  def l1Miss_mtlbHit_part1(previous_instructions, current_instruction)
    puts "(and "
      @data_builder.L1interMTLB.collect{|d| "bv#{d.tagset}[#{$TAGSETLEN}]"}.notisin( current_instruction.tagset )
      puts "(or false ";@data_builder.V0.each{|v| puts"(and true";mtlb_useful( v, previous_instructions, current_instruction, ">=" );puts")" };puts ")"
    puts ")"
  end
  
  def l1Miss_mtlbHit_part2(previous_instructions, current_instruction)
    puts "(or false "
      @data_builder.L1interMTLB.each{|d|
        puts "(and true"
        l1_useful( d.tagset, previous_instructions, current_instruction, "<" )
        mtlb_useful( d.vpnd2, previous_instructions, current_instruction, ">=" )
        puts ")"}
    puts ")"
  end
  
  def l1Miss_mtlbHit_part3(previous_instructions, current_instruction)
    if ! previous_instructions.empty?
      puts "(and "
        previous_instructions.collect{|i| i.vpnd2}.isin(current_instruction.vpnd2)
        puts "(or false "
          @data_builder.L1interTLB.each{|d|
              puts "(and true ";l1_useful( d.tagset, previous_instructions, current_instruction, "<" );puts ")" }
          @data_builder.L1interTLB.collect{|d| "bv#{d.tagset}[#{$TAGSETLEN}]"}.notisin(current_instruction.tagset)
        puts ")"
      puts ")"
    end
  end
  
  def l1Miss_mtlbMiss(previous_instructions, current_instruction)
    puts "(and "
      previous_instructions.collect{|i| i.tagset}.notisin(current_instruction.tagset)      
      previous_instructions.collect{|i| i.vpnd2}.notisin(current_instruction.vpnd2)      
      puts "(or ";(1..4).each{|p| send("l1Miss_mtlbMiss_part#{p}", previous_instructions, current_instruction)};puts ")"
    puts ")"
  end
  
  def l1Miss_mtlbMiss_part1(previous_instructions, current_instruction)
    puts "(and "
      @data_builder.L1interNotMTLB.collect{|d| "bv#{d.tagset}[#{$TAGSETLEN}]"}.notisin(current_instruction.tagset)
      @data_builder.notV0.collect{|v| "bv#{v}[#{$VPNdiv2LEN}]"}.isin(current_instruction.vpnd2)
    puts ")"
  end
  
  def l1Miss_mtlbMiss_part2(previous_instructions, current_instruction)
    puts "(or false "
      @data_builder.L1interNotMTLB.each{|d|
        puts"(and (= #{current_instruction.vpnd2} bv#{d.vpnd2}[#{$VPNdiv2LEN}]) "        
          l1_useful( d.tagset, previous_instructions, current_instruction, "<" )
        puts")" }
    puts ")"
  end
  
  def l1Miss_mtlbMiss_part3(previous_instructions, current_instruction)
    puts "(and true "
        @data_builder.L1interMTLB.collect{|d| "bv#{d.tagset}[#{$TAGSETLEN}]"}.notisin(current_instruction.tagset)
        puts "(or false";@data_builder.V0.each{|v| puts "(and true ";mtlb_useful(v, previous_instructions, current_instruction, "<");puts ")" };puts ")"
    puts ")"
  end
  
  def l1Miss_mtlbMiss_part4(previous_instructions, current_instruction)
    puts "(or false "
      @data_builder.L1interMTLB.each{|d|
          puts "(and true "
          l1_useful( d.tagset, previous_instructions, current_instruction, "<" )
          mtlb_useful( d.vpnd2, previous_instructions, current_instruction, "<" )
          puts ")" }
    puts ")"
  end
  

  
  def procedures_preparations doc
    previous_objects = []
  
    @instruction_objects = Hash.new 
    @l1Hits = []
    @mtlbHits = []
    doc.elements.each('template/instruction/situation/memory'){ |memory|
      cacheTestSituation = memory.elements['cache'].attributes['id']
      microTLBSituation = memory.elements['microtlb'].attributes['id']
        
      # ввести переменную для этой тестовой ситуации и записать ее в SMT-LIB
      tagset = "_ts#{@unique_counter += 1}"
      vpnd2 = "_vpnd#{@unique_counter += 1}"
      virtual_address = "_va#{@unique_counter += 1}"
      
      puts ":extrafuns (( #{tagset} #{$TAGSETTYPE} ) " + 
                        " (#{vpnd2} #{$VPNd2TYPE}) " +
                        " (#{virtual_address} BitVec[64]))"

      instruction_object = Instruction.new
      instruction_object.tagset = tagset
      instruction_object.vpnd2 = vpnd2
      instruction_object.virtual_address = virtual_address
      
      # vpnd2 is bits of virtual_address
      puts ":assumption"
      puts "(= #{getVPNdiv2 virtual_address} #{vpnd2})"
      
      # разных vpn'в не более количества строк TLB, но для маленьких экспериментов это всегда верно
  
      #если совпадают vpn, то совпадают pfn
      if microTLBSituation != "mtlbMiss"
        previous_objects.each{|o|
            puts ":assumption"
            puts "(implies (and (= #{vpnd2} #{o.vpnd2}) " +
            "(= #{getOddBit virtual_address} #{getOddBit o.virtual_address} )) "
            puts "(= #{o.tagset} #{tagset}))"
        }
      end

      # разных vpn'в не более количества строк TLB, но для маленьких экспериментов это всегда верно
  
      # сделать ограничения для cacheTS >< microTLBS и выдать их на out
      puts ":assumption"
      send("#{cacheTestSituation}_#{microTLBSituation}", previous_objects, instruction_object)
      
      @l1Hits << instruction_object if cacheTestSituation == "l1Hit"
      @mtlbHits << instruction_object if microTLBSituation == "mtlbHit"
      
      vpn_pfn instruction_object
      
      @instruction_objects.merge!( {memory.parent.parent => instruction_object} )
      previous_objects << instruction_object
    }
  end

end

class MIPS_MirrorSolver < MIPS_Solver

def l1Hit( init_tagsets, previous_tagsets, current_tagset )
  mirror init_tagsets, previous_tagsets, current_tagset, ">"
end

def l1Miss( init_tagsets, previous_tagsets, current_tagset )
  mirror init_tagsets, previous_tagsets, current_tagset, "<="
end

# x is current tag
# _T is a list of initial tags
def mirror( _T, previous_tagsets, x, mirrrelation )
  puts "(and "
    x.isin _T
    
    puts "(#{mirrrelation} #{$L1ASSOC} (+ "
          _T.inject([]){|xs,t|
                puts "(ite (and "
                x.isin xs # это упрощение x ~isin {t_i,...,t_m,x_1,...,x_n} 
                x.notisin previous_tagsets
                puts "(= #{x.region} #{t.region})"
                puts ") 1 0)"
                xs + [t]
          }
          t1 = _T
          t2 = previous_tagsets
          previous_tagsets.each{|x_i|
                puts "(ite (and "
                x.notisin t2
                puts "(= #{x.region} #{x_i.region})"
                t1.reverse.each{|t_i|
                  puts "(and (= bit0 (bvcomp #{x_i} #{t_i})) (or (= #{x} #{t_i}) "                
                }
                puts "false" + ")" * (2*t1.length)
                puts ") 1 0)"
                t1 = t1 + [t2[0]] if t2.length > 0
                t2 = t2[1..t2.length-1] if t2.length > 0
          }
    puts "))"
  puts ")"
end

def mtlbHit(previous_instructions, current_instruction)
  puts "(or false "
    previous_instructions.map{|i| i.vpnd2}.isin( current_instruction.vpnd2 )

    @data_builder.V0.each{|v|
      puts "(and true ";mtlb_useful(v, previous_instructions, current_instruction, ">=");puts")"
    }
  puts")"
end

def mtlbMiss(previous_instructions, current_instruction)
  puts "(and true"
    previous_instructions.map{|i| i.vpnd2}.notisin( current_instruction.vpnd2 )
    puts "(or false "
      @data_builder.notV0.map{|v| "bv#{v}[#{$VPNdiv2LEN}]"}.isin( current_instruction.vpnd2 )
      
      @data_builder.V0.each{|v|
        puts "(and true ";mtlb_useful(v, previous_instructions, current_instruction, "<");puts")"
      }      
    puts ")"
  puts ")"
end

def procedures_preparations doc
  raise "Please define initlength" if $initlength.nil? || $initlength == 0
  
  previous_objects = []
  previous_tagsets = []
  @mtlbHits = []
  
  init_tagsets = Array.new($initlength){"_its#{@unique_counter += 1}"}
  puts ":extrafuns(" + init_tagsets.map{|t| "( #{t} #{$TAGSETTYPE} )" }.join + ")"
  puts ":assumption"
  puts "(distinct #{init_tagsets.join(' ')})"
#  init_tagsets.distinct
  
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
         
    instruction_object = Instruction.new
    instruction_object.tagset = tagset
    instruction_object.vpnd2 = vpnd2
    instruction_object.virtual_address = virtual_address

    
    # vpnd2 is bits of virtual_address
    puts ":assumption"
    puts "(= #{getVPNdiv2 virtual_address} #{vpnd2})"
    
    # разных vpn'в не более количества строк TLB, но для маленьких экспериментов это всегда верно

    #каждый init_vpnd2 не может быть одновременно использован с разными oddbit
    #если совпадают vpn, то совпадают pfn
      if microTLBSituation != "mtlbMiss"
        previous_objects.each{|o|
            puts ":assumption"
            puts "(implies (and (= #{vpnd2} #{o.vpnd2}) " +
            "(= #{getOddBit virtual_address} #{getOddBit o.virtual_address} )) "
            puts "(= #{o.tagset} #{tagset}))"
        }
      end
    
    # сделать ограничения для cacheTS >< microTLBS и выдать их на out
    puts ":assumption"
    send(cacheTestSituation, init_tagsets, previous_tagsets, tagset)
    puts ":assumption"
    send(microTLBSituation, previous_objects, instruction_object )
    
    @instruction_objects.merge!( {memory.parent.parent => instruction_object} )
    previous_objects << instruction_object
    previous_tagsets << tagset
    
    @mtlbHits << instruction_object if microTLBSituation == "mtlbHit"
  }
  
  @pairs.each{|p| puts ":assumption";puts "(= bit0 (bvcomp #{previous_tagsets[p[0]].region} #{previous_tagsets[p[1]].region}))" }
end

def solve3(template, data, pairs )
  @pairs = pairs || []
  solve2(template, data)
end

end

class MIPS_FullMirrorSolver < MIPS_MirrorSolver

#def mtlbHit( init_vpnd2s, previous_vpnd2s, current_vpnd2 )
#  (init_vpnd2s + previous_vpnd2s).last($TLBASSOC).isin(current_vpnd2)
#end
#
#def mtlbMiss( init_vpnd2s, previous_vpnd2s, current_vpnd2 )
#  puts "(and "
#    (init_vpnd2s + previous_vpnd2s).
#      first(init_vpnd2s.length + previous_vpnd2s.length-$TLBASSOC).
#        isin(current_vpnd2)
#    previous_vpnd2s.last($TLBASSOC).notisin(current_vpnd2)
#  puts ")"
#end

def mtlbHit( init_vpnd2s, previous_vpnd2s, current_vpnd2 )
  tmirror init_vpnd2s, previous_vpnd2s, current_vpnd2, ">"
end

def mtlbMiss( init_vpnd2s, previous_vpnd2s, current_vpnd2 )
  tmirror init_vpnd2s, previous_vpnd2s, current_vpnd2, "<="
end

# x is current tag
# _T is a list of initial tags
def tmirror( _T, previous_tagsets, x, mirrrelation )
  puts "(and "
    x.isin _T
    
    puts "(#{mirrrelation} #{$TLBASSOC} (+ "
          _T.inject([]){|xs,t|
                puts "(ite (and "
                x.isin xs # это упрощение x ~isin {t_i,...,t_m,x_1,...,x_n} 
                x.notisin previous_tagsets
                puts ") 1 0)"
                xs + [t]
          }
          t1 = _T
          t2 = previous_tagsets
          previous_tagsets.each{|x_i|
                puts "(ite (and "
                x.notisin t2
                t1.reverse.each{|t_i|
                  puts "(and (= bit0 (bvcomp #{x_i} #{t_i})) (or (= #{x} #{t_i}) "                
                }
                puts "false" + ")" * (2*t1.length)
                puts ") 1 0)"
                t1 = t1 + [t2[0]] if t2.length > 0
                t2 = t2[1..t2.length-1] if t2.length > 0
          }
    puts "))"
  puts ")"
end

def to_h(keys, values)
  Hash[*([keys, values].transpose.flatten)]
end

def procedures_preparations doc
  raise "Please define initlength" if $initlength.nil? || $initlength == 0
  raise "Please define initlength_mtlb" if $initlength_mtlb.nil? || $initlength_mtlb == 0
  
  previous_tagsets = []
  previous_objects = []
  previous_vpnd2s = []
  
  init_tagsets = Array.new($initlength){"_its#{@unique_counter += 1}"}
  puts ":extrafuns(" + init_tagsets.map{|t| "( #{t} #{$TAGSETTYPE} )" }.join + ")"
  puts ":assumption"
  puts "(distinct #{init_tagsets.join(' ')})"
#  init_tagsets.distinct
  
  init_vpnd2s = Array.new($initlength_mtlb){"_ivpnd#{@unique_counter += 1}"}
  puts ":extrafuns(" + init_vpnd2s.map{|t| "( #{t} #{$VPNd2TYPE} )" }.join + ")"
  puts ":assumption"
  puts "(distinct #{init_vpnd2s.join(' ')})"
#  init_vpnd2s.distinct
  
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
    if microTLBSituation != "mtlbMiss"
      previous_objects.each{|o|
          puts ":assumption"
          puts "(implies (= #{vpnd2} #{o.vpnd2}) " +
          "(and (= #{getOddBit virtual_address} #{getOddBit o.virtual_address} ) " +
          "(= #{o.tagset} #{tagset})))" # упрощено для случая fullmirror
      }
    end
    
    # сделать ограничения для cacheTS >< microTLBS и выдать их на out
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


