raise "Define N" if ARGV[0].nil?
N = ARGV[0].to_i

require "rexml/document"

$LOAD_PATH << "C:\\Documents and Settings\\kornevgen2\\My Documents\\dissertation\\TeSLa-XP\\src\\ruby"

require "tesla-mips"
require "tesla"

full_solver = MIPS_FullMirrorSolver.new
i = -1

ALL = Array.new(2*N){|aLL_w_index| "x#{aLL_w_index}" }
Ins1 = Array.new(N){ ["LW", "SB"] }.flatten
Ins4 = Array.new(N) { ["load", "store"] }.flatten



10000.times{

xs = Array.new(ALL.length){ ALL[rand(ALL.length)]}

ins2 = Array.new(N){ ["l1Hit", "l1Miss"][rand(2)] }
ins3 = Array.new(N){ ["mtlbHit", "mtlbMiss"][rand(2)] }

i += 1

#File.delete("out#{i-4}.smt") if File.exists?("out#{i-4}.smt")
#File.delete("tmpl#{i-4}.xml") if File.exists?("tmpl#{i-4}.xml")
template_file = "tmpl#{i%10}.xml"



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

max = N * $L1ASSOC + ins2.select{|ii| ii == "l1Miss"}.length
if ins2.select{|ii| ii == "l1Miss"}.length == 0
  min = 1
else
  min = $L1ASSOC + 1
end

$initlength_mtlb = N * $TLBASSOC + ins3.select{|ii| ii == "mtlbMiss"}.length

  (min..max).to_a.reverse.each{|m|
    i += 1
    $initlength = m
    f1 = Runner.new.run( full_solver, i%10, template_file, "B:/data.xml" )
    raise RuntimeError, "Full Timeout: initlength = #{m} " if f1.include?("timeout")
  }
}