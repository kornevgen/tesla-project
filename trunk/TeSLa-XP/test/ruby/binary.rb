require "rexml/document"

$LOAD_PATH << "C:\\Documents and Settings\\kornevgen2\\My Documents\\dissertation\\TeSLa-XP\\src\\ruby"

require "tesla.rb"
require "tesla-mips.rb"

mirror_solver = MIPS_MirrorSolver.new

[4,8].each{|l1assoc|
$L1ASSOC = l1assoc
$TLBASSOC = l1assoc
(2..$L1ASSOC).each{|n|

i = 0
ALL = Array.new(2*n){|aLL_index| "x#{aLL_index}" }
Ins1 = Array.new(n){ ["LW", "SB"] }.flatten
Ins4 = Array.new(n) { ["load", "store"] }.flatten

histogram = Hash.new
(1 .. ALL.length * ($L1ASSOC + 1)/2 ).each{|t|
  histogram.merge!({t => 0})
}

1500.times{

xs = Array.new(ALL.length){ ALL[rand(ALL.length)]}

ins2 = Array.new(n){ ["l1Hit", "l1Miss"][rand(2)] }
ins3 = Array.new(n){ ["mtlbHit", "mtlbMiss"][rand(2)] }

i += 1

File.delete("out#{i-4}.smt") if File.exists?("out#{i-4}.smt")
File.delete("tmpl#{i-4}.xml") if File.exists?("tmpl#{i-4}.xml")
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
File.delete("data#{i-4}.xml") if File.exists?("data#{i-4}.xml")
data_file = "data#{i}.xml"
File.new(data_file, "w")
File.open(data_file, "w"){|f|
  f.puts "<data>"
  
  f.puts "<tlb>"
  vpns = []
  44.times{
    begin
      vpn = rand(2**18)
    end while vpns.include?(vpn)    
    vpns << vpn
    
    f.puts "<line range='0' vpndiv2='#{vpn}' mask='0'" +
           " pfn0='#{(p0 = rand(2**$PFNLEN))}' CCA0='Cached'" + 
           " pfn1='#{(p1 = rand(2**$PFNLEN))}' CCA1='Cached' />"
  }
  f.puts "</tlb>"
  
  # microtlb
  f.puts "<microtlb>"
  $TLBASSOC.times{
    begin
      vpn = rand(2**18)
    end while vpns.include?(vpn)    
    vpns << vpn
    
    f.puts "<line range='0' vpndiv2='#{vpn}' mask='0'" +
           " pfn0='#{(p0 = rand(2**$PFNLEN))}' CCA0='Cached'" + 
           " pfn1='#{(p1 = rand(2**$PFNLEN))}' CCA1='Cached' />"
  }
  f.puts "</microtlb>"
  
  f.puts "</data>"
}

# пробуем максимальное значение $initlength
$initlength = n * $L1ASSOC + ins2.select{|ii| ii == "l1Miss"}.length
pairs = []
(0..n-2).each{|f|
    (f+1..n-1).each{|s|
        pairs << [f,s] if rand(4) == 0
    }
}
f1 = Runner.new.run( mirror_solver, i, template_file, data_file, pairs )
#raise "Full Timeout" if f1.include?("timeout")
next if f1.include?("timeout")
next if f1.include?("unsat")

# начинаем искать минимальный initlength
max = $initlength
if ins2.select{|ii| ii == "l1Miss"}.length == 0
	min = 1
else
	min = 1 + $L1ASSOC
end
puts "initlength : #{min} .. #{max}"

while max >= min
  $initlength = (max + min)/2
  puts "initlength = #{$initlength}:"
  f1 = Runner.new.run( mirror_solver, i, template_file, data_file, pairs )
  next if f1.include?("timeout")
  #raise "Full Timeout" if f1.include?("timeout")
  if f1.include?("unsat")
    min = $initlength + 1
  else
    max = $initlength - 1
  end
end
histogram.merge!({min => histogram[min]+1})


histogram.each{|length, count| puts "#{length} => #{count} раз" if count > 0 } if i % 3 == 0

}

File.new("E:/histogrammN#{n}W#{$L1ASSOC}.txt", "w")
File.open("E:/histogrammN#{n}W#{$L1ASSOC}.txt", "w"){|f|
histogram.each{|length, count| f.puts "#{length} => #{count} раз" if count > 0 } }

}}