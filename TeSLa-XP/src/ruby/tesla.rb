require "rexml/document"

class Solver

attr_accessor :synonyms

def constraintsfrom_assume( operator, full_context )
  puts ":assumption"
  puts send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) 
end

def binary_constraint( sign, operator, full_context )
  "(#{sign} " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) + " " +
      send("constraintsfrom_#{operator.elements[2].name}", operator.elements[2], full_context) + ")"
end

def constraintsfrom_and( operator, full_context )
  binary_constraint "and", operator, full_context
end

def constraintsfrom_or( operator, full_context )
  binary_constraint "or", operator, full_context
end

def constraintsfrom_predicate( operator, full_context )
  if operator.attributes['name'] == "wordvalue"
    localvar = "_local_var_#{@unique_counter += 1}"
    "(let (#{localvar} " + 
        send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) +
     ") (= " + 
        "(extract[63:32] #{localvar}) " + 
        "(repeat[32] (extract[31:31] #{localvar}))" + 
     "))"
  else
    raise "unknown predicate \'#{operator.attributes['name']}\'"
  end
end

def constraintsfrom_noteq( operator, full_context )
  "(= bit0 #{binary_constraint('bvcomp', operator, full_context)})"
end

def constraintsfrom_eq( operator, full_context )
  binary_constraint('=', operator, full_context)
end

def constraintsfrom_uless( operator, full_context )
  binary_constraint('bvult', operator, full_context )
end

def constraintsfrom_bit( operator, full_context )
  "(extract[#{operator.attributes['index']}:#{operator.attributes['index']}] " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) + ")"
end

def constraintsfrom_bits( operator, full_context )
  "(extract[#{operator.attributes['end']}:#{operator.attributes['start']}] " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) + ")"
end

def constraintsfrom_concat( operator, full_context )
  binary_constraint('concat', operator, full_context)
end

def constraintsfrom_power( operator, full_context )
  "(repeat[#{operator.attributes['index']}] " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) + ")"
end

def constraintsfrom_sum( operator, full_context )
  binary_constraint('bvadd', operator, full_context)
end

def constraintsfrom_sign_extend( operator, full_context )
  length = send("length_#{operator.elements[1].name}", operator.elements[1])
  "(sign_extend[#{operator.attributes['size'].to_i - length }] " +
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) + ")"
end

def constraintsfrom_var( operator, full_context )
  v = full_context[operator.text]
  raise "cannot find synonym for variable '#{operator.text}'" if v == nil
  v
end

def constraintsfrom_constant( operator, full_context )
  "bv#{operator.text}[#{operator.attributes['length']}]"
end

def process_instruction(instruction)
  @instruction = instruction

  raise "please define $instructionsPath" if $instructionsPath == nil
 
  #TODO ввести поддержку композиции ветвей
  path = $instructionsPath + instruction.attributes['name'] +
  "/" + instruction.elements['situation'].elements['branch'].attributes['name'] + ".xml"
  test_situation = REXML::Document.new File.new(path)  

  reverse_synonyms = Hash[*test_situation.get_elements('situation/argument').  ## перевести в текст??
      zip(instruction.get_elements('argument')).flatten]
      
  test_situation.elements.each('//[@state="result"]'){|arg|
       new_name = "#{@synonyms[reverse_synonyms[arg].attributes['name']]}_X"
       puts ":extrafuns (( #{new_name} BitVec[#{arg.attributes['length']}] ))"
  }

  full_context = Hash.new
  @lengths_context = Hash.new
  reverse_synonyms.each{|tsarg, insarg|
      full_context[tsarg.attributes['name']] = @synonyms[insarg.attributes['name']] +
                        (tsarg.attributes["state"] == "result" ? "_X" : "" )
      @lengths_context[tsarg.attributes['name']] = @varlengths[insarg.attributes['name']]
  }
  
  @current_externals = Hash.new
  @current_externals_length = Hash.new
  # traverse operators
  test_situation.elements.each('situation/*[not(starts-with(name(),"argument"))]'){|operator|
      send( "constraintsfrom_#{operator.name}", operator, full_context )
  }
  instruction.elements.each('external'){|external|
      id = external.attributes['id']
      raise "undefined external id '#{id}' in situation #{instruction.attributes['name']}::#{instruction.elements['situation'].elements['branch'].attributes['name']}" if ! @current_externals.has_key?(id)
      raise "redefined name '#{external.attributes['name']}'" if @synonyms.has_key? external.attributes['name']
      @synonyms.merge!({external.attributes['name'] => @current_externals[id]})
      @varlengths.merge!({external.attributes['name'] => @current_externals_length[id]})
  }
  
  #TODO поддержка идентификаторов инструкций
  
  test_situation.elements.each('//[@state="result"]'){|arg|
       @synonyms[reverse_synonyms[arg].text] = "#{@synonyms[reverse_synonyms[arg].text]}_X"
  }
end

def process_assume(assume)
  @instruction = assume
  
  raise "please define $instructionsPath" if $instructionsPath == nil
  
  #TODO ввести поддержку композиции ветвей
  path = $instructionsPath + assume.attributes['name'] + ".xml"
  test_situation = REXML::Document.new File.new(path)

  reverse_synonyms = Hash[*test_situation.get_elements('situation/argument').  ## перевести в текст??
      zip(assume.get_elements('argument')).flatten]

  raise "assume mustn't have result arguments" if test_situation.get_elements('//[@state="result"]').length > 0

  full_context = Hash.new
  @lengths_context = Hash.new
  reverse_synonyms.each{|tsarg, insarg|
      full_context[tsarg.attributes['name']] = @synonyms[insarg.attributes['name']]
      @lengths_context[tsarg.attributes['name']] = @varlengths[insarg.attributes['name']]
  }
#  full_context.merge! @synonyms #TODO зачем это?! ведь имена шаблона недоступны внутри ситуации!
#  @lengths_context.merge! @varlengths #TODO ---//---

#raise "full_context: #{full_context.to_a.collect{|a| '['+a[0]+'=>'+a[1]+']'}}"

  # traverse operators
  @current_externals = Hash.new
  test_situation.elements.each('situation/*[not(starts-with(name(),"argument"))]'){|operator|
      send( "constraintsfrom_#{operator.name}", operator, full_context )
  }
  assume.elements.each('external'){|external|
      id = external.attributes['id']
      raise "undefined external id '#{id}' in situation #{assume.attributes['name']}::#{instruction.elements['situation'].elements['branch'].attributes['name']}" if ! @current_externals.has_key?(id)
      raise "redefined name '#{external.attributes['name']}'" if @synonyms.has_key? external.attributes['name']
      @synonyms.merge!({external.attributes['name'] => @current_externals[id]})
      @varlengths.merge!({external.attributes['name'] => @current_externals_length[id]})
  }
  
  #TODO поддержка идентификаторов инструкций
end

def constraintsfrom_let( operator, full_context )
  raise "let: name or id must be specified" if operator.attributes['name'] == nil && operator.attributes['id'] == nil
  
  bitlen = send("length_#{operator.elements[1].name}", operator.elements[1] )
  new_name = "_localvar_#{@unique_counter += 1}"
  full_context.merge!( {operator.attributes['name'] => new_name } ) if operator.attributes['name'] != nil
  @lengths_context[operator.attributes['name']] = bitlen if operator.attributes['name'] != nil
  
  if operator.attributes['id'] != nil
    raise "id '#{operator.attributes['id']}' is already used" if @current_externals.has_key?(operator.attributes['id'])
    @current_externals.merge!({operator.attributes['id'] => new_name})
    @current_externals_length.merge!({operator.attributes['id'] => bitlen})
  end
  
  puts ":extrafuns (( #{new_name} BitVec[#{bitlen}] ))"
  puts ":assumption"
  puts "(= #{new_name} " + 
      send("constraintsfrom_#{operator.elements[1].name}", operator.elements[1], full_context) + ")"
end

def constraintsfrom_procedure( operator, full_context )
  operator.elements.each('argument/new') {|n|
      raise "Redefinition of '#{n.text}'" if full_context.has_key?(n.text)
      new_name = "_localvar_#{@unique_counter += 1}"
      full_context.merge!( {n.text => new_name } )
      @lengths_context[n.text] = n.attributes['length'].to_i
      puts ":extrafuns((#{new_name} BitVec[#{n.attributes['length']}]))"
  }
  
  send("constraintsfrom_#{operator.attributes['name']}", operator, full_context)
end

def length_equalbinary(operator)
  len1 = send("length_#{operator.elements[1].name}", operator.elements[1])
  len2 = send("length_#{operator.elements[2].name}", operator.elements[2])
  raise "arguments must have equal bitlengths (now #{len1} and #{len2})" if len1 != len2
  len1  
end

def length_sum(operator)
  length_equalbinary(operator)
end

def length_concat(operator)
  send("length_#{operator.elements[1].name}", operator.elements[1]) +
  send("length_#{operator.elements[2].name}", operator.elements[2])
end

def length_power(operator)
  send("length_#{operator.elements[1].name}", operator.elements[1]) *
    operator.attributes['index'].to_i
end

def length_bit(operator)
  1
end

def length_bits(operator)
  operator.attributes['end'].to_i - operator.attributes['start'].to_i + 1
end

def length_sign_extend(operator)
  operator.attributes['size'].to_i
end

def length_var(operator)
  l = @lengths_context[operator.text]
  raise "unknown length for '#{operator.text}'" if l == nil
  l
end

def length_constant(node)
  node.attributes['length'].to_i
end

def length_DOUBLEWORD
  64
end

def length_WORD
  32
end

def length_HALFWORD
  16
end

def length_BYTE
  8
end

def solve1 template
  solve template
end

def solve template
#  template = File.new template_file
  
  raise "file not found: #{template}" if template.nil?
  
  @unique_counter = 0
  @memory_accesses = []
  
  puts "(benchmark tesla"
#  puts ":logic QF_BV"
  
  doc = REXML::Document.new template

  procedures_preparations doc

  @synonyms = Hash.new
  @varlengths = Hash.new
  # ввести определения регистров и констант
  doc.elements.each('template/register | template/constant') { |reg|
      puts ":extrafuns (( #{reg.attributes['name']}_X BitVec[#{reg.attributes['length']}] ))"  
      @synonyms[reg.attributes['name']] = "#{reg.attributes['name']}_X"
      @varlengths[reg.attributes['name']] = reg.attributes['length'].to_i
  }
  
  #TODO сделать поддержку идентификаторов инструкций
  
  # странслировать определения тестовых ситуаций
  doc.elements.each('template/instruction') {|instruction|
      (instruction.attributes["clone"]||"1").to_i.times{|i|
          process_instruction instruction
      }
  }

  # странслировать допущения
  doc.elements.each('template/assume') {|assume|
          process_assume assume
  }
  
  puts ")"
end

def procedures_preparations document
end

end

require 'tempfile'

class Runner

  def run( solver, i, *params )
    orig = $stdout
    smt_file = File.new( "out#{i}.smt", "w" )
    $stdout = smt_file
    solver.send("solve#{params.length}", *params)
    $stdout = orig
    smt_file.close
    #File.open(smt_file.path).each{|s| puts s }
    output = `z3 /m #{smt_file.path} /T:60`
    puts output
    (puts "out#{i}.smt: timeout";return "timeout") if output.include?("timeout")
    (puts "out#{i}.smt: unsat"; return "unsat") if output.include?("unsat")
    puts "out#{i}.smt: sat" if !output.include?("unsat") && !output.include?("timeout")
    #smt_file.unlink
    hash = Hash[*(output[0..-5].scan(/(.+) -> bv(\d+)\[\d+\]/)).flatten]
    # using solver.synonyms, replace corresponding names in hash (localvar -> id)
    solver.synonyms.each_pair{|k,v| puts "%%% #{k} +> #{v}"}
    
    solver.synonyms.each_pair{|k,v|
      #raise "??? (#{k} +> #{v})" if hash[v] == nil
      hash[k] = hash[v] if k != nil
      #TODO разобраться, откуда берется ключ nil и с ним значение "_X"
    }

    #puts ">>>#{hash.to_a.collect{|a| '['+a[0]+'=>'+a[1]+']'}}"
    hash.each_pair{|k,v| puts ">>> #{k} +> #{v}"}
    hash
  end
end