require "rexml/document"

$LOAD_PATH << "../../src/ruby"

require "tesla.rb"
require "tesla-mips.rb"

mirror_solver = MIPS_MirrorSolver.new
i = 0
ALL = ["x1", "y1", "z1", "u1", "x2", "y2", "z2", "u2"]

histogram = Hash.new
(1 .. ALL.length * ($L1ASSOC + 1)/2 ).each{|t|
  histogram.merge!({t => 0})
}

10000.times{

xs = Array.new(ALL.length){ ALL[rand(ALL.length)]}
cts1 = ["l1Hit", "l1Miss"][rand(2)]
mts1 = ["mtlbHit", "mtlbMiss"][rand(2)]
cts2 = ["l1Hit", "l1Miss"][rand(2)]
mts2 = ["mtlbHit", "mtlbMiss"][rand(2)]
cts3 = ["l1Hit", "l1Miss"][rand(2)]
mts3 = ["mtlbHit", "mtlbMiss"][rand(2)]
cts4 = ["l1Hit", "l1Miss"][rand(2)]
mts4 = ["mtlbHit", "mtlbMiss"][rand(2)]

i += 1

template_file = "template/#{(i-1)%10}.xml"



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

f.puts('<instruction name="LW">')
f.puts("<argument>#{xs[0]}</argument>")
f.puts("<argument>#{xs[1]}</argument>")
f.puts('<argument>c</argument>')
f.puts('<situation><branch name="load"/>')
f.puts('<memory>')
f.puts("<cache id='#{cts1}' />")
f.puts("<microtlb id='#{mts1}' />")
f.puts('</memory>')
f.puts('</situation>')
f.puts('</instruction>')

f.puts('<instruction name="SB">')
f.puts("<argument>#{xs[2]}</argument>")
f.puts("<argument>#{xs[3]}</argument>")
f.puts('<argument>c</argument>')
f.puts('<situation><branch name="store"/>')
f.puts('<memory>')
f.puts("<cache id='#{cts2}' />")
f.puts("<microtlb id='#{mts2}' />")
f.puts('</memory>')
f.puts('</situation>')
f.puts('</instruction>')

f.puts('<instruction name="LW">')
f.puts("<argument>#{xs[4]}</argument>")
f.puts("<argument>#{xs[5]}</argument>")
f.puts('<argument>c</argument>')
f.puts('<situation><branch name="load"/>')
f.puts('<memory>')
f.puts("<cache id='#{cts3}' />")
f.puts("<microtlb id='#{mts3}' />")
f.puts('</memory>')
f.puts('</situation>')
f.puts('</instruction>')

f.puts('<instruction name="SB">')
f.puts("<argument>#{xs[6]}</argument>")
f.puts("<argument>#{xs[7]}</argument>")
f.puts('<argument>c</argument>')
f.puts('<situation><branch name="store"/>')
f.puts('<memory>')
f.puts("<cache id='#{cts4}' />")
f.puts("<microtlb id='#{mts4}' />")
f.puts('</memory>')
f.puts('</situation>')
f.puts('</instruction>')

f.puts("</template>")
}

#сгенерировать новый data.xml
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

# пробуем максимальное значение $initlength
$initlength = ALL.length/2 * $L1ASSOC + [cts1,cts2,cts3,cts4].reject{|ii| ii == "l1Miss"}.length
f1 = Runner.new.run( mirror_solver, template_file, data_file )
next if f1.include?("unsat") || f1.include?("timeout")

# начинаем искать минимальный initlength
max = $initlength
min = 1
puts "initlength : #{min} .. #{max}"

while max >= min
  $initlength = (max + min)/2
  puts "initlength = #{$initlength}:"
  f1 = Runner.new.run( mirror_solver, template_file, data_file )
  if f1.include?("unsat") || f1.include?("timeout")
    min = $initlength + 1
  else
    max = $initlength - 1
  end
end
histogram.merge!({min => histogram[min]+1})


histogram.each{|length, count| puts "#{length} => #{count} раз" } if i % 3 == 0

}
histogram.each{|length, count| puts "#{length} => #{count} раз" }
