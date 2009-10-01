N = (ARGV[0]||"3").to_i

require "rexml/document"

$LOAD_PATH << "../../src/ruby"

require "tesla-mips"
require "tesla"

combined_solver = MIPS_CombinedSolver.new
mirror_solver = MIPS_MirrorSolver.new
sat_all = 0
incompatible_combined_only = 0
incompatible_all = 0
i = 0
startTime = Time.now

ALL = Array.new(2*N){|aLL_w_index| "x#{aLL_w_index}" }
Ins1 = Array.new(N){ ["LW", "SB"] }.flatten
Ins4 = Array.new(N) { ["load", "store"] }.flatten



10000.times{

xs = Array.new(ALL.length){ ALL[rand(ALL.length)]}

ins2 = Array.new(N){ ["l1Hit", "l1Miss"][rand(2)] }
ins3 = Array.new(N){ ["mtlbHit", "mtlbMiss"][rand(2)] }

i += 1

template_file = "template/#{(i-1)%10}.xml"

$initlength = ALL.length/2 * $L1ASSOC + ins2.reject{|ii| ii == "l1Miss"}.length


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

#������������� ����� data.xml
data_file = "data/data#{(i-1)%10}.xml"
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
f = Runner.new.run( combined_solver, template_file, data_file )
e = Time.now
puts "����������: #{e-s} �."

sat_all += 1 if ! f.include?("unsat")

s = Time.now
f1 = Runner.new.run( mirror_solver, template_file, data_file )
e = Time.now
puts "����������: #{e-s} �."

raise RuntimeError, "Combined sat /\ Mirror unsat" if ! f.include?("unsat") && f1.include?("unsat")

incompatible_combined_only += 1 if !f1.include?("unsat") && !f1.include?("timeout") && f.include?("unsat")
incompatible_all += 1 if f1.include?("unsat") || (f1.include?("timeout") && f.include?("unsat"))

aaa = sat_all + incompatible_all + incompatible_combined_only
puts "���: #{aaa}"
puts "combined: #{sat_all * 100 / aaa} % ( #{sat_all} / #{aaa} )"
puts "incompatible combined only: #{incompatible_combined_only * 100 / aaa} % ( #{incompatible_combined_only} / #{aaa} )"
puts "incompatible: #{incompatible_all * 100 / aaa} % ( #{incompatible_all} / #{aaa} )" 
puts "��� ����������: #{sat_all * 100 / (sat_all + incompatible_combined_only)} % ( #{sat_all} / #{(sat_all + incompatible_combined_only)} )" \
              if sat_all + incompatible_combined_only > 0
puts "=======NEXT===DEPENDENCIES======================="
puts ""

}

endTime = Time.now

#puts "���: #{sat_all + incompatible_all}"
#puts "combined: #{sat_all * 100 / (sat_all + incompatible_all)} %"
#puts "incompatible combined only: #{incompatible_combined_only * 100 / (sat_all + incompatible_all)} %"
#puts "incompatible: #{incompatible_all * 100 / (sat_all + incompatible_all)} %"
#puts "��� ����������: #{sat_all * 100 / (sat_all + incompatible_combined_only)} %"

duration = endTime - startTime
puts "����� �����: #{duration.to_s} �."