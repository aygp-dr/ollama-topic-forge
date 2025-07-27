# Proper Citations and Links

## 1. Design by Contract - Bertrand Meyer

**Citation:**
Meyer, Bertrand. "Applying 'Design by Contract'." *IEEE Computer*, vol. 25, no. 10, October 1992, pp. 40-51.

**DOI/URL:** https://doi.org/10.1109/2.161279

**Alternative URL:** https://se.inf.ethz.ch/~meyer/publications/computer/contract.pdf

**Brief Description:**
This seminal paper presents methodological guidelines for object-oriented software construction using Design by Contract (DbC). Meyer shows how DbC improves software reliability by treating interfaces between software modules as formal contracts with preconditions, postconditions, and invariants. The methodology underlies the design of the Eiffel programming language and has influenced modern approaches to software specification and testing.

**Note:** The concept was first introduced in Meyer's earlier works starting in 1986 and fully developed in his book "Object-Oriented Software Construction" (1988, revised 1997).

## 2. Property-Based Testing - QuickCheck Papers

**Citation:**
Claessen, Koen, and John Hughes. "QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs." *Proceedings of the Fifth ACM SIGPLAN International Conference on Functional Programming (ICFP '00)*, ACM Press, 2000, pp. 268-279.

**DOI/URL:** https://doi.org/10.1145/351240.351266

**Alternative URL:** https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf

**Brief Description:**
This paper introduces QuickCheck, a tool that aids programmers in formulating and testing properties of programs through automatic testing on random input. QuickCheck pioneered property-based testing, where tests are written as general properties that should hold for all inputs, rather than specific test cases. The approach has been widely adopted and ported to many programming languages, fundamentally changing how developers think about testing.

## 3. Experimental Systems - MIT AI Lab Technical Reports

**Note:** "Experimental Systems" appears to be a general reference to the body of work produced at MIT AI Lab rather than a specific technical report. Key examples include:

**Notable Technical Reports:**
- Winograd, Terry. "Procedures as a Representation for Data in a Computer Program for Understanding Natural Language." MIT AI Lab Technical Report 235, 1971. (SHRDLU system)
- Winston, Patrick H. "Learning Structural Descriptions from Examples." MIT AI Lab Technical Report 231, 1970.
- Various reports by Marvin Minsky and John McCarthy documenting early AI experimental systems (1959-1970s)

**Repository:** Many MIT AI Lab technical reports (1959-2004) are available through MIT's DSpace: https://dspace.mit.edu/

**Brief Description:**
The MIT AI Lab (founded by McCarthy and Minsky in 1959) produced numerous technical reports documenting experimental AI systems. These reports covered pioneering work in natural language processing (SHRDLU), computer vision (Marr's work), symbolic computation (MACSYMA), and machine learning. They represent a crucial historical record of early AI research and experimental methodology.

## 4. TLA+ Specifications - Leslie Lamport

**Citation:**
Lamport, Leslie. "The Temporal Logic of Actions." *ACM Transactions on Programming Languages and Systems (TOPLAS)*, vol. 16, no. 3, May 1994, pp. 872-923.

**DOI/URL:** https://doi.org/10.1145/177492.177726

**Alternative URLs:**
- https://lamport.azurewebsites.net/pubs/lamport-actions.pdf
- https://www.microsoft.com/en-us/research/publication/the-temporal-logic-of-actions/

**Brief Description:**
This paper introduces TLA (Temporal Logic of Actions), a logic for specifying and reasoning about concurrent systems. TLA uses actions (formulas with primed and unprimed variables) in temporal formulas to describe state transitions. It provides the mathematical foundation for TLA+, a formal specification language widely used for designing and verifying distributed systems. The approach enables expressing both system specifications and implementations in the same logic, with refinement expressed as logical implication.

## 5. "Parse, Don't Validate" - Alexis King

**Citation:**
King, Alexis. "Parse, Don't Validate." *Personal Blog*, 5 November 2019.

**URL:** https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

**Brief Description:**
This influential blog post articulates a fundamental principle of type-driven design: use parsing (which produces refined types) instead of validation (which merely checks but doesn't refine types). King argues for writing "total functions" that leverage the type system to make illegal states unrepresentable. The post provides practical guidance for implementing this pattern in Haskell and has inspired implementations in many other languages. The core insight is that parsing should transform less-structured input into more-structured output that carries proof of validity in its type.