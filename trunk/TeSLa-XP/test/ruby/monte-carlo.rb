

require "rexml/document"

require "tesla-mips"
require "tesla"

combined_solver = MIPS_CombinedSolver.new
mirror_solver = MIPS_FullMirrorSolver.new

[8,16].each{|l1assoc|
$L1ASSOC = l1assoc
$TLBASSOC = l1assoc
(2..$L1ASSOC).each{|n|

next if l1assoc == 8 && n == 2

sat_all = 0
incompatible_combined_only = 0
incompatible_all = 0
i = 0
startTime = Time.now

ALL = Array.new(2*n){|aLL_w_index| "x#{aLL_w_index}" }
Ins1 = Array.new(n){ ["LW", "SB"] }.flatten
Ins4 = Array.new(n) { ["load", "store"] }.flatten
maxtime = 0
sumtime = 0


while sat_all + incompatible_combined_only < 1500 do

xs = Array.new(2*n){ ALL[rand(2*n)]}

ins2 = Array.new(n){ ["l1Hit", "l1Miss"][rand(2)] }
ins3 = Array.new(n){ ["mtlbHit", "mtlbMiss"][rand(2)] }

i += 1

i = 1 if i > 10

template_file = "tmpl#{i}.xml"



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

(0..n-1).each{|ins|
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
data_file = "data#{i}.xml"
File.new(data_file, "w")
pfns=[]
File.open(data_file, "w"){|f|
  f.puts "<data>"
  
  f.puts "<cache level='1' mode='DATA'>"
  (0..2**$SETLEN - 1).each{|set|
    f.puts "<set value='#{set}'>"
    tags = Array.new
      (1..$L1ASSOC).each{|tagn|
          begin
            tag = rand(2**24)
          end until ! tags.include? tag
          tags << tag
          f.puts "<tag value='#{tag}' />"
      }
#    pfns = pfns + tags  
    f.puts "</set>"
  }
  f.puts "</cache>"
#это "неподготовленные" (произвольные) данные  
  f.puts "<tlb>"
  vpns = []
  44.times{
    begin
      vpn = rand(2**18)
    end while vpns.include?(vpn)    
    vpns << vpn
    
    p0 = ( if rand(2) == 0 && !pfns.empty?
            idx = rand( pfns.length )
            pfns.delete_at( idx )
        else
            rand(2**$PFNLEN)
        end )
    
    p1 = ( if rand(2) == 0 && !pfns.empty?
            idx = rand( pfns.length )
            pfns.delete_at( idx )
        else
            rand(2**$PFNLEN)
        end )
    
    f.puts "<line range='0' vpndiv2='#{vpn}' mask='0'" +
           " pfn0='#{p0}' CCA0='Cached'" + 
           " pfn1='#{p1}' CCA1='Cached' />"
  }
  f.puts "</tlb>"
  
  # microtlb
  f.puts "<microtlb>"
  $TLBASSOC.times{
    begin
      vpn = rand(2**18)
    end while vpns.include?(vpn)    
    vpns << vpn
    
    p0 = ( if rand(2) == 0 && !pfns.empty?
            idx = rand( pfns.length )
            pfns.delete_at( idx )
        else
            rand(2**$PFNLEN)
        end )
    
    p1 = ( if rand(2) == 0 && !pfns.empty?
            idx = rand( pfns.length )
            pfns.delete_at( idx )
        else
            rand(2**$PFNLEN)
        end )
    
    f.puts "<line range='0' vpndiv2='#{vpn}' mask='0'" +
           " pfn0='#{p0}' CCA0='Cached'" + 
           " pfn1='#{p1}' CCA1='Cached' />"
  }
  f.puts "</microtlb>"
  
  f.puts "</data>"
}

s = Time.now
f = Runner.new.run( combined_solver, i, template_file, data_file )
e = Time.now
sovm = e - s
puts "combined: #{sovm} s."

next if sovm > 60

(f = "unsat";sovm = 0) if f.include?("timeout")

#$initlength = n * $L1ASSOC + ins2.select{|ii| ii == "l1Miss"}.length
#$initlength_mtlb = n * $TLBASSOC + ins3.select{|ii| ii == "mtlbMiss"}.length

m2 = ins2.select{|ii| ii == "l1Miss"}.length
m3 = ins3.select{|ii| ii == "mtlbMiss"}.length
$initlength = $L1ASSOC * [m2, 1].min + 1
$initlength_mtlb = $TLBASSOC * [m3, 1].min + 1


#raise RuntimeError, "Combined Timeout" if f.include?("timeout")
s = Time.now
f1 = Runner.new.run( mirror_solver, i, template_file, data_file )
e = Time.now
puts "mirror: #{e-s} s."

next if f1.include?("timeout")



if f1.include?("unsat") && !f.include?("unsat")
	$initlength = $L1ASSOC * [m2, 1].min + $L1ASSOC
	$initlength_mtlb = $TLBASSOC * [m3, 1].min + $TLBASSOC

	s = Time.now
	f1 = Runner.new.run( mirror_solver, i, template_file, data_file )
	e = Time.now
	puts "mirror: #{e-s} s."

	next if f1.include?("timeout")
end

sumtime += sovm
maxtime = sovm if sovm > maxtime

sat_all += 1 if ! f.include?("unsat")

raise RuntimeError, "FullMirror Timeout" if f1.include?("timeout")
raise RuntimeError, "Combined sat /\\ Mirror unsat" if ! f.include?("unsat") && f1.include?("unsat")

incompatible_combined_only += 1 if !f1.include?("unsat") && !f1.include?("timeout") && f.include?("unsat")
incompatible_all += 1 if f1.include?("unsat") || (f1.include?("timeout") && f.include?("unsat"))

aaa = sat_all + incompatible_all + incompatible_combined_only
puts "all: #{aaa}"
puts "combined: #{sat_all * 100 / aaa} % ( #{sat_all} / #{aaa} )"
puts "incompatible combined only: #{incompatible_combined_only * 100 / aaa} % ( #{incompatible_combined_only} / #{aaa} )"
puts "incompatible: #{incompatible_all * 100 / aaa} % ( #{incompatible_all} / #{aaa} )" 
puts "KPD совместной: #{sat_all * 100 / (sat_all + incompatible_combined_only)} % ( #{sat_all} / #{(sat_all + incompatible_combined_only)} )" \
              if sat_all + incompatible_combined_only > 0
puts "max time = #{maxtime} c."
puts "middle time = #{sumtime / aaa} c."
puts "=======NEXT===DEPENDENCIES======================="
puts ""

end

result_file = "C:/kpdN#{n}W#{l1assoc}.txt"
File.new(result_file, "w")
File.open(result_file, "w"){|f|
aaa = sat_all + incompatible_all + incompatible_combined_only
f.puts "all: #{aaa}"
f.puts "combined: #{sat_all * 100 / aaa} % ( #{sat_all} / #{aaa} )"
f.puts "incompatible combined only: #{incompatible_combined_only * 100 / aaa} % ( #{incompatible_combined_only} / #{aaa} )"
f.puts "incompatible: #{incompatible_all * 100 / aaa} % ( #{incompatible_all} / #{aaa} )" 
f.puts "KPD совместной: #{sat_all * 100 / (sat_all + incompatible_combined_only)} % ( #{sat_all} / #{(sat_all + incompatible_combined_only)} )" \
              if sat_all + incompatible_combined_only > 0
f.puts "max time = #{maxtime} c."
f.puts "middle time = #{sumtime / aaa} c."
}

}}