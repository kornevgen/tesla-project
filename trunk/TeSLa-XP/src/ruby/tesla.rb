### эта версия корректно работает только со случаями Cached >< Mapped!

require "rexml/document"

class DataBuilder
  
  def initialize
    @tagsets = Hash.new
    dataxml = REXML::Document.new File.new(ARGV[1])
    dataxml.elements.each("data/cache/set") do |set|
        set.elements.each_with_index{ |tag,delta|
              tagvalue = tag.attributes["value"]
              @tagsets[tagvalue.to_i * 128 + set.attributes["value"].to_i ] = delta }
    end
    
    @micropfns = dataxml.elements.collect("data/tlb/microtlb/pfn"){ |pfn|
                      pfn.attributes["value"].to_i }
    
    @nonmicropfns = dataxml.elements.collect("data/tlb/pfn"){ |pfn|
                      pfn.attributes["value"].to_i }
    
    @pfns = @micropfns + @nonmicropfns
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
end

class Instruction
  attr_accessor :virtual_address
  attr_accessor :tagset
  attr_accessor :phys_after_translation
  attr_accessor :phys_before_memaccess
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
  @VIRTUALLEN = 64
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

# инициализировать выходной файл (поток?)

# инициализировать входной файл с шаблоном (поток?)

def solve
  raise "please add the first argument for xml with test template" if ARGV[0].nil?
  raise "please add the second argument for xml with initial microprocessor state" if ARGV[1].nil?
  template = File.new ARGV[0]
  
  raise "file not found: #{ARGV[0]}" if template.nil?
  
  @unique_counter = 0
  @data_builder = DataBuilder.new
  
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
    
    puts ":extrafuns ((#{ins.virtual_address} BitVec[#{@VIRTUALLEN}]))"
  end
  
  
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