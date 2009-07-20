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
    
    @micropfns = dataxml.elements.each("data/tlb/microtlb/pfn"){ |pfn|
                      pfn.attributes["value"].to_i }
    
    @nonmicropfns = dataxml.elements.each("data/tlb/pfn"){ |pfn|
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
  
  def getTail(lambda,delta)
    setvalue = lambda % 128
    
    tail = Array.new 
    
    dataxml = REXML::Document.new File.new(ARGV[1])
    dataxml.elements.each("data/cache/set") do |set|
      if set.attributes["value"] == setvalue.to_s
        tail << set.elements.values_at(delta..@L1ASSOC-1).each{ |tag|
            (tag.attribute["value"].to_i * 128 + setvalue.to_i).to_s }.join
      end
    end
  end
end

class Solver

def initialize
  @unique_counter = 0
  @TAGSETLEN = 31
  @TAGSETTYPE = "BitVec[#{@TAGSETLEN}]"
  @L1ASSOC = 4
end

def getPfn(tagset)
  "(extract[30:7] #{tagset})"
end

def getRegion(tagset)
  "(extract[6:0] #{tagset})"
end

def l1Hit_mtlbHit_part1(previous_tagsets, current_tagset)
  if previous_tagsets.empty?
    ""
  else
    "(and "
    
        "(or false" + previous_tagsets.each { |tagset|
            " (= #{current_tagset} #{tagset} )" }.join + ")"    
            
        "(or false" + previous_tagsets.each { |tagset|
            " (= #{getPfn current_tagset} #{getPfn tagset}) " }.join + ")"
            
     ")"
  end
end

def l1Hit_mtlbHit_part2(previous_tagsets, current_tagset)

  a2 = "(or false" + previous_tagsets.each { |tagset|
             " (= #{getPfn current_tagset} #{getPfn tagset}) " }.join + ")"

  if @data_builder.LinterPFN.empty?
    a2
  else
    
    a1 = "(or false" <<    
    
    @data_builder.LinterPFN.
       select{ |lambda,delta| delta <= @L1ASSOC - previous_tagsets.length }.
       each_key{ |lambda| "(= #{current_tagset} bv#{lambda}[#{@TAGSETLEN}])" }.join +
    
    @data_builder.LinterPFN.
      select{ |lambda,delta| delta > L1ASSOC - previous_tagsets.length }.
      each{ |lambda,delta|
        "(and "
          "(= #{current_tagset} #{lambda})"
          "(>= #{@L1ASSOC - delta} (+ 0 " +
            previous_tagsets.each_index { |i|
              "(ite" 
              if @l1Hits.contains current_tagset
                  "(and " 
                    "(or false "
                        @data_builder.getTail(lambda,delta).each{ |ltail|
                        "(= #{previous_tagsets[i]} bv#{ltail}[#{@TAGSETLEN}])" }.join
                    ")"
                    previous_tagsets.values_at(0..i-1).
                      each{|t| "(/= #{previous_tagsets[i]} #{t})" }.join
                  " ) " 
              else
                "(= #{getRegion(current_tagset)} #{getRegion(tagset)})"
              end
              " 1 0)"
            }.join
          "))"
         ")"
      }.join +
    ")"
    
    "(and #{a1} #{a2})"
  end
end

def l1Hit_mtlbHit(previous_tagsets, current_tagset)
  
  puts ":assumption"
  puts "(or false " <<
    "#{l1Hit_mtlbHit_part1 previous_tagsets, current_tagset}" <<
    "#{l1Hit_mtlbHit_part2 previous_tagsets, current_tagset})"
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
        l1Hit_mtlbHit previous_tagsets, tagset
      else
        #TODO
      end
    else
      if microTLBSituation == "mtlbHit"
        #TODO
      else
        #TODO
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