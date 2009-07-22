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
    if @LinterPFN.nil?
      @LinterPFN = @tagsets.select{|tagset,delta|
                    @pfns.include?(tagset/128) }
    end
    @LinterPFN
  end
  
  def L
    @tagsets
  end
  
  def M
     @pfns
  end
  
  def getMTail(delta)
    @pfns.values_at(delta+1..@pfns.length-1)
  end
  
  def LinterM
    #TODO
  end
  
  def LinterPFNminusM
    #TODO
  end
end

#TODO упростить реализацию тестовых ситуаций вначале с учетом добавленных методов

#TODO реализовать tlb_is_displaced и парный метод

class Solver

def initialize
  @unique_counter = 0
  @TAGSETLEN = 31
  @PFNLEN = 24
  @TAGSETTYPE = "BitVec[#{@TAGSETLEN}]"
  @L1ASSOC = 4
  @TLBASSOC = 4
end

def getPfn(tagset)
  "(extract[30:7] #{tagset})"
end

def getRegion(tagset)
  "(extract[6:0] #{tagset})"
end

def l1Hit_mtlbHit_part1(previous_tagsets, current_tagset)
    "(and " +
    
        "(or false" + previous_tagsets.collect { |tagset|
            " (= #{current_tagset} #{tagset} )" }.join + ")" +    
            
        "(or false" + previous_tagsets.collect { |tagset|
            " (= #{getPfn current_tagset} #{getPfn tagset}) " }.join + ")" +
            
     ")"
end

def l1Hit_mtlbHit_part2(previous_tagsets, current_tagset)

  a2 = "(or false" + previous_tagsets.collect { |tagset|
             " (= #{getPfn current_tagset} #{getPfn tagset}) " }.join + ")"

  if @data_builder.LinterPFN.empty?
    a2
  else
   "(and " +
    "(or false" +
    (
    @data_builder.LinterPFN.
       select{ |lambda,delta| delta <= @L1ASSOC - previous_tagsets.length }.
       collect{ |lambda,delta| "(= #{current_tagset} bv#{lambda}[#{@TAGSETLEN}])" } +
    
    @data_builder.LinterPFN.
      select{ |lambda,delta| delta > @L1ASSOC - previous_tagsets.length }.
      collect{ |lambda,delta|
        "(and " +
          "(= #{current_tagset} #{lambda})" +
          "(>= #{@L1ASSOC - delta} (+ 0 " +
            (0..previous_tagsets.length-1).collect { |i|
              "(ite" + 
              if @l1Hits.include? previous_tagsets[i]
                  "(and " +
                    "(or false " +
                        @data_builder.getTail(lambda,delta).collect{ |ltail|
                        "(= #{previous_tagsets[i]} bv#{ltail}[#{@TAGSETLEN}]) " }.join
                    ")" +
                    previous_tagsets.values_at(0..i-1).collect{|t|
                        "(/= #{previous_tagsets[i]} #{t})" }.join +
                  " ) " 
              else
                "(= #{getRegion(current_tagset)} #{getRegion(tagset)})"
              end +
              " 1 0)"
            }.join +
          "))" +
         ")"
      }
    ).join +
    ")" +
    
    "#{a2})"
  end
end

def l1Hit_mtlbHit_part3(previous_tagsets, current_tagset)
  
   "(and " +
    "(or false" +

# delta \in 0..@TLBASSOC - previous.length - 1 ==> только pfn(c_t) = mu_delta
# delta \in @TLBASSOC - previous.length .. @TLBASSOC - prevtlbmisses.length-1 ==> pfn(c_t) = mu_delta /\ полезности
      @data_builder.M.values_at(0 .. @TLBASSOC - previous_tagsets.length-1).
          collect{|m| " (= #{getPfn current_tagset} bv#{m}[#{@PFNLEN}])" }.join +
          
      @data_builder.M.values_at(@TLBASSOC - previous_tagsets.length ..
        @TLBASSOC - previous_tagsets.select{|t| ! @mtlbHits.include? t }.length - 1).
          collect{|m|
            delta = @data_builder.M.index(m)
            "(and " +
              "(= #{getPfn current_tagset} bv#{m}[#{@PFNLEN}]) " +
              "(>= #{@TLBASSOC - delta - previous_tagsets.select{|t| ! @mtlbHits.include? t }.length} (+ 0 " +
              (0..previous_tagsets.length-1).collect{|i| if @mtlbHits.include? previous_tagsets[i]
                "(and " +
                    "(or " +
                    @data_builder.getMTail(delta+1).collect{|mtail|
                      "(= #{getPfn previous_tagsets[i]} bv#{mtail}[#{@PFNLEN}]) "
                    }.join +
                    ")" +
                    
                    previous_tagsets.values_at(0..i-1).collect{|t|
                      "(= bit0 (bvcomp #{getPfn previous_tagsets[i]} #{getPfn t} )) "
                    }.join +
                ")"
              end }.compact.collect{|f| "(ite #{f} 1 0)"}.join + 
            ")))"
          }.join +
          
    ")" +
    
    "(or false" + previous_tagsets.collect { |tagset|
          " (= #{current_tagset} #{tagset}) " }.join + ")" +
    
    ")"
end

def l1Hit_mtlbHit_part4(previous_tagsets, current_tagset)
  "(or false " +
      @data_builder.LinterM.select{|lambda,delta|
          @pfns.index(lambda/128) <= @TLBASSOC - previous_tagsets.length + @mtlbHits.length - 1}.
      collect{|lambda,delta|
        delta_T = @pfns.index(lambda/128)
        "(and (= #{current_tagset} bv#{lambda}[#{@TAGSETLEN}])" +
            cache_tagset_is_not_displaced_yet(previous_tagsets, current_tagset, lambda, delta) +
            (if delta_T > @TLBASSOC - previous_tagsets.length
                "(>= #{@TLBASSOC - delta_T - previous_tagsets.length + @mtlbHits.length} (+ 0 " +
                      (0..previous_tagsets.length-1).collect{|i| if @mtlbHits.include? previous_tagsets[i]
                      "(and " +
                          "(or " +
                          @data_builder.getMTail(delta_T+1).collect{|mtail|
                            "(= #{getPfn previous_tagsets[i]} bv#{mtail}[#{@PFNLEN}]) "
                          }.join +
                          ")" +
                          
                          previous_tagsets.values_at(0..i-1).collect{|t|
                            "(= bit0 (bvcomp #{getPfn previous_tagsets[i]} #{getPfn t} )) "
                          }.join +
                      ")"
                    end }.compact.collect{|f| "(ite #{f} 1 0)"}.join +                     
                "))"
            else ""
            end) +
        ")"
      }.join
  ")"
end

def l1Hit_mtlbHit(previous_tagsets, current_tagset)
  puts ":assumption"
  puts "(or false "
  puts l1Hit_mtlbHit_part1 previous_tagsets, current_tagset
  puts l1Hit_mtlbHit_part2 previous_tagsets, current_tagset
  puts l1Hit_mtlbHit_part3 previous_tagsets, current_tagset
  puts l1Hit_mtlbHit_part4 previous_tagsets, current_tagset
  puts ")"
end

def l1Hit_mtlbMiss_part1(previous_tagsets, current_tagset)
  "(or false " +
      @data_builder.LinterPFNminusM.collect{|lambda,delta|
        "(and " +
              "(= #{current_tagset} bv#{lambda}[#{@TAGSETLEN}])" +
              cache_tagset_is_not_displaced_yet(previous_tagsets, current_tagset, lambda, delta) +
        ")" }.join +
  ")" 
end

def l1Hit_mtlbMiss_part2(previous_tagsets, current_tagset)
  "(or false " +
      @data_builder.LinterM.collect{|lambda,delta|
        "(and " +
              "(= #{current_tagset} bv#{lambda}[#{@TAGSETLEN}])" +
              cache_tagset_is_not_displaced_yet(previous_tagsets, current_tagset, lambda, delta) +
              tlb_pfn_is_displaced_already(previous_tagsets, current_tagset, lambda/128) +
        ")" }.join +
  ")" 
end


def cache_tagset_is_not_displaced_yet(previous_tagsets, current_tagset, lambda, delta)
          if delta > @L1ASSOC - previous_tagsets.length
              "(>= #{@L1ASSOC - delta} (+ 0 " +
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
          if delta > @L1ASSOC - previous_tagsets.length
              "(< #{@L1ASSOC - delta} (+ 0 " +
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

def l1Hit_mtlbMiss(previous_tagsets, tagset)
  previous_tagsets.each{|t|
    puts ":assumption"
    puts "(= bit0 (bvcomp #{getRegion t} #{getRegion tagset}))"
  }
  
  puts ":assumption"
  puts "(or false "
  puts l1Hit_mtlbMiss_part1 previous_tagsets, tagset
  puts l1Hit_mtlbMiss_part2 previous_tagsets, tagset
  puts ")"
end

def l1Miss_mtlbHit_part1 previous_tagsets, tagset
  "(and " + 
      @data_builder.LinterM.collect{|lambda,delta|
          " (/= #{tagset} bv#{lambda}[#{@TAGSETLEN}])"
      }.join +
      
      "(or " + @data_builder.M.collect{|m|
          "(and " +
              "(= #{getRegion tagset} bv#{m}[#{@PFNLEN}])" +
              tlb_pfn_is_not_displaced_yet(previous_tagsets, tagset, m) +
          ")"
      }.join + ")" +
  ")"
end

def l1Miss_mtlbHit_part2 previous_tagsets, tagset
  "(and " +
      @data_builder.LinterPFN.collect{|lambda,delta|
          "(/= #{tagset} bv#{lambda}[#{@TAGSETLEN}])"
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
      } + ")"
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
      } + ")"
end

def l1Miss_mtlbMiss_part1 previous_tagsets, tagset
  "(and true " + 
      @data_builder.LinterPFNminusM.collect{|lambda,delta|
          " (/= #{tagset} bv#{lambda}[#{@TAGSETLEN}])"
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
            "(/= #{tagset} bv#{lambda}[#{@TAGSETLEN}])"
        }.join +
        
        "(or " +
            @data_builder.M.collect{|m|
                "(and (= #{getRegion tagset} bv#{m}[#{@PFNLEN}]) " +
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
      } + ")"
end

def l1Miss_mtlbHit( previous_tagsets, tagset )
  previous_tagsets.each{|t|
    puts ":assumption"
    puts "(/= #{t} #{tagset})"
  }
  
  puts ":assumption"
  puts "(or false "
  puts l1Miss_mtlbHit_part1 previous_tagsets, tagset
  puts l1Miss_mtlbHit_part2 previous_tagsets, tagset
  puts l1Miss_mtlbHit_part3 previous_tagsets, tagset
  puts l1Miss_mtlbHit_part4 previous_tagsets, tagset
  puts ")"  
end

def l1Miss_mtlbMiss( previous_tagsets, tagset )
  previous_tagsets.each{|t|
    puts ":assumption"
    puts "(= bit0 (bvcomp #{getRegion t} #{getRegion tagset}))"
  }
  
  puts ":assumption"
  puts "(or false "
  puts l1Miss_mtlbMiss_part1 previous_tagsets, tagset
  puts l1Miss_mtlbMiss_part2 previous_tagsets, tagset
  puts l1Miss_mtlbMiss_part3 previous_tagsets, tagset
  puts l1Miss_mtlbMiss_part4 previous_tagsets, tagset
  puts ")"
end

# инициализировать выходной файл (поток?)

# инициализировать входной файл с шаблоном (поток?)

def solve
  
  if ARGV[0].nil?
    raise "please add the first argument for xml with test template"
  end
  
  if ARGV[1].nil?
    raise "please add the second argument for xml with initial microprocessor state"
  end
  
  template = File.new ARGV[0]
  
  if template.nil?
    raise "file not found: #{ARGV[0]}"
  end
  
  @unique_counter = 0
  
  @data_builder = DataBuilder.new
  
  puts "(benchmark tesla"
  puts ":logic QF_BV"
  
  doc = REXML::Document.new template
  
  previous_tagsets = Array.new
  
  @l1Hits = Array.new
  @mtlbHits = Array.new
  
  doc.elements.each('template/instruction/situation/memory') do |memory|
    cacheTestSituation = memory.elements['cache'].attributes['id']
    microTLBSituation = memory.elements['microtlb'].attributes['id']
      
    # ввести переменную для этой тестовой ситуации и записать ее в SMT-LIB
    @unique_counter = @unique_counter + 1
    tagset = "tagset" + @unique_counter.to_s 
    puts ":extrafuns (( #{tagset} #{@TAGSETTYPE} ))"
     
    # сделать ограничения для cacheTS >< microTLBS и выдать их на out
    if cacheTestSituation == "l1Hit"
      @l1Hits << tagset
      if microTLBSituation == "mtlbHit"
        @mtlbHits << tagset
        l1Hit_mtlbHit previous_tagsets, tagset
      else
        l1Hit_mtlbMiss previous_tagsets, tagset
      end
    else
      if microTLBSituation == "mtlbHit"
        @mtlbHits << tagset
        l1Miss_mtlbHit previous_tagsets, tagset
      else
        l1Miss_mtlbMiss previous_tagsets, tagset
      end
    end
    
    previous_tagsets << tagset
  end
  
  # open as XML and
  
  
  # задача на тестовые ситуации
  
  puts ")"
  end
end

Solver.new.solve