# Regularna pełnia
#### Program, który dla wyrażenia regularnego r nad jednoelementowym alfabetem Σ = {a} sprawdza, czy r generuje język pełny, tj. czy L(r) = a<sup>*</sup>
### Allowed constants
##### - a: literal character
##### - e: empty string (epsilon)
### Allowed operations
##### - R*: Kleene star
##### - RS: concatenation
##### - R+S: alternation
### Additional information
##### - Spaces allowed
##### - Redundant and nested parentheses allowed
### Examplary regular expressions and their results
##### - aaa*+aa+(((((a+e)))))+aaa*+aa -> True
##### - (((e)))+ea(ee+e(e+a*)) -> True
##### - a + e + aa* -> True
##### - e+a+aa+aaa+aaa+aaaaaa* -> False
##### - (((a)))+eaae+aa* -> False
##### - a+a(a+ae)a* -> False
