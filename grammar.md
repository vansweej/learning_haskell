Simple grammar to parse and do simple computations with:

<exp> ::= <exp> + <term> | <exp> - <term> | <term> 
<term> ::= <term> * <power> | <term> / <power> | <power> 
<power> ::= <factor> ^ <power> | <factor>
<factor> ::= ( <exp> ) | <int>
<int> ::= <digit> <int> | <digit>
<digit> ::= 0|1|2|3|4|5|6|7|8|9