N = (ARGV[0]||"3").to_i

require "rexml/document"

#$LOAD_PATH << "../../src/ruby"
$LOAD_PATH << "C:\\Documents and Settings\\kornevgen2\\My Documents\\dissertation\\TeSLa-XP\\src\\ruby"

require "tesla-mips"
require "tesla"

mirror_solver = MIPS_FullMirrorSolver.new

template_file = "0.xml"

$initlength = 3
$initlength_mtlb = 3
i = 0
#сгенерировать новый data.xml
begin
  data_file = "dataN.xml"
File.new(data_file, "w")
File.open(data_file, "w"){|f|
  f.puts "<data>"
  
  f.puts "<cache level='1' mode='DATA'>"
  (0..2**7-1).each{|set|
    f.puts "<set value='#{set}'>"
    tags = Array.new
      (1..4).each{|tagn|
          begin
            tag = rand(2**24)
          end until ! tags.include? tag
          tags << tag
          f.puts "<tag value='#{tag}' />"
      }
    f.puts "</set>"
  }
  f.puts "</cache>"
  
  f.puts "<tlb>"
  # content
  f.puts "<content>"
  vpns = []
  lines0 = []
  lines1 = []
  48.times{
    begin
      vpn = rand(2**18)
    end while vpns.include?(vpn)    
    vpns << vpn
    
    f.puts "<line range='0' vpndiv2='#{vpn}' mask='0'" +
           " pfn0='#{(p0 = rand(2**$PFNLEN))}' CCA0='Cached'" + 
           " pfn1='#{(p1 = rand(2**$PFNLEN))}' CCA1='Cached' />"
           
    lines0 << p0 if lines0.length < 4
    lines1 << p1 if lines1.length < 4
  }
  f.puts "</content>"
  
  # microtlb
  f.puts "<microtlb>"
  (lines0 + lines1).each{|iii| f.puts "<pfn value='#{iii}' />" }
  f.puts "</microtlb>"
  
  f.puts "</tlb>"
  
  f.puts "</data>"
}

s = Time.now
f1 = Runner.new.run( mirror_solver, (i = (i+1)%100), template_file, data_file )
e = Time.now

duration = e - s
puts "время: #{duration.to_s} с."

end while !f1.include?("timeout")