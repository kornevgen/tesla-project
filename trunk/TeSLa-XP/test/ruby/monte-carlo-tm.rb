raise "Define N" if ARGV[0].nil?
N = ARGV[0].to_i

require "rexml/document"

$LOAD_PATH << "C:\\Documents and Settings\\kornevgen\\Desktop\\tesla.2008.09.24\\TeSLa-XP\\src\\ruby"

require "tesla-mips"
require "tesla"

mirror_solver = MIPS_MirrorSolver.new
full_solver = MIPS_FullMirrorSolver.new
timeout = 0
all = 0
i = 0

ALL = Array.new(2*N){|aLL_w_index| "x#{aLL_w_index}" }
Ins1 = Array.new(N){ ["LW", "SB"] }.flatten
Ins4 = Array.new(N) { ["load", "store"] }.flatten



10000.times{

xs = Array.new(ALL.length){ ALL[rand(ALL.length)]}

ins2 = Array.new(N){ ["l1Hit", "l1Miss"][rand(2)] }
ins3 = Array.new(N){ ["mtlbHit", "mtlbMiss"][rand(2)] }

i += 1

File.delete("out#{i-4}.smt") if File.exists?("out#{i-4}.smt")
File.delete("tmpl#{i-4}.xml") if File.exists?("tmpl#{i-4}.xml")
template_file = "tmpl#{i}.xml"

$initlength = N * $L1ASSOC + ins2.select{|ii| ii == "l1Hit"}.length
$initlength_mtlb = N * $TLBASSOC + ins3.select{|ii| ii == "mtlbHit"}.length


File.new(template_file, "w")
File.open(template_file, "w"){|f|
f.puts("<template>")
ALL.each{|s| f.puts("<register id='#{s}' length='64' />") }
f.puts('<constant id="c" length="16" />')
#  <instruction name="ADD">
#    <argument>x</argument>
#    <argument>y</argument>
#    <argument>z</argument>
#    <situation name="overflow" />
#  </instruction>

(0..N-1).each{|ins|
  f.puts("<instruction name='#{Ins1[ins]}'>")
  f.puts("<argument>#{xs[2*ins]}</argument>")
  f.puts("<argument>#{xs[2*ins+1]}</argument>")
  f.puts('<argument>c</argument>')
  f.puts("<situation><branch name='#{Ins4[ins]}'/>")
  f.puts('<memory>')
  f.puts("<cache id='#{ins2[ins]}' />")
  f.puts("<microtlb id='#{ins3[ins]}' />")
  f.puts('</memory>')
  f.puts('</situation>')
  f.puts('</instruction>')
}
f.puts("</template>")
}

#сгенерировать новый data.xml
File.delete("data#{i-4}.xml") if File.exists?("data#{i-4}.xml")
data_file = "data#{i}.xml"
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

f = Runner.new.run( mirror_solver, i, template_file, data_file )
f1 = Runner.new.run( full_solver, i, template_file, data_file )

raise RuntimeError, "Mirror sat /\\ Full unsat" if ! f.include?("unsat") && f1.include?("unsat")
raise RuntimeError, "Mirror unsat /\\ Full sat" if f.include?("unsat") && ! f1.include?("unsat")
raise RuntimeError, "Full Timeout" if f1.include?("timeout")

timeout += 1 if f.include?("timeout")
all += 1

puts "КПД: #{(all-timeout) * 100 / all} %"
puts "=======NEXT===DEPENDENCIES======================="
puts ""

}