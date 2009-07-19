require "rexml/document"

# инициализировать выходной файл (поток?)

# инициализировать входной файл с шаблоном (поток?)

template = File.new ARGV[0]

if template.nil?
  raise "file not found: #{ARGV[0]}"
end

puts "(benchmark tesla"
puts ":logic QF_BV"

doc = REXML::Document.new template

tagset_number = 0

TAGSETTYPE = "BitVec[31]"

doc.elements.each('template/instruction/situation/memory') do |memory|
  cacheTestSituation = memory.elements['cache'].attributes['id']
  microTLBSituation = memory.elements['microtlb'].attributes['id']
    
  # ввести переменную для этой тестовой ситуации и записать ее в SMT-LIB
  tagset_number = tagset_number + 1
  tagset = "tagset" + tagset_number.to_s 
  puts ":extrafuns (( #{tagset} #{TAGSETTYPE} ))"
   
  # сделать ограничения для cacheTS >< microTLBS и выдать их на out
  if cacheTestSituation == "l1Hit"
    if microTLBSituation == "mtlbHit"
      puts ":assumption"
      #puts formula 
    end
  end
end

# open as XML and


# задача на тестовые ситуации

puts ")"