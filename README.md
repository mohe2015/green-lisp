# green-lisp
An eco-friendly lisp

# My own [https://en.wikipedia.org/wiki/Programming_language programming language]
* Check all languages from [https://en.wikipedia.org/wiki/List_of_programming_languages https://en.wikipedia.org/wiki/List_of_programming_languages] – I’m at I now

# Goals
* it’s a Lisp
* A compiler for x86_64 should be written first, as we could be self-hosting then. After that avr, then idk
* Code rewriting abilities (with types?, based on AST?): like optima: (chain sth (modal “hide”)) → (hide-modal sth)
* Security: User system (functions have call permissions and can only be called by “users” with that permission). Remote procedure calls also work that way. Therefore almost no custom protocols are required. [https://en.wikipedia.org/wiki/E_(programming_language) https://en.wikipedia.org/wiki/E_(programming_language)]
* macros (for DSLs etc)
* Primary targets: [https://en.wikipedia.org/wiki/Computer_security Se][https://en.wikipedia.org/wiki/Computer_security curity], embedded, real-time
* [https://en.wikipedia.org/wiki/Eiffel_(programming_language https://en.wikipedia.org/wiki/Eiffel_(programming_language][https://en.wikipedia.org/wiki/Eiffel_(programming_language) )] good design ideas
* always progn (if else)
* explicit lazy evaluation (using macro)
* Fast (explicit concurrency (over multiple devices, etc.), prevent death-locks)
* [https://en.wikipedia.org/wiki/Automated_theorem_proving Automated_theorem_proving]: [https://en.wikipedia.org/wiki/Formal_verification Formal_verification], [https://en.wikipedia.org/wiki/Coq https://en.wikipedia.org/wiki/Coq], ACL2
* [https://en.wikipedia.org/wiki/Model–view–controller Model-view-controller]?
* TODO [https://en.wikipedia.org/wiki/Semantics_(computer_science) Semantics_(computer_science)]
* Decentralized: Peer-to-peer, 
* TODO null
* TODO memory safety
* TODO [https://en.wikipedia.org/wiki/Off-by-one_error Off-by-one_error]
* TODO [https://en.wikipedia.org/wiki/Region-based_memory_management Region-based_memory_management]
* [https://en.wikipedia.org/wiki/Design_by_contract Design_by_contract] (using a DSL)
* Lowest level: Assembly language
* Long variable/function/class/... names (e.g. counter instead of cnt)
* [https://en.wikipedia.org/wiki/Type_system Type_system]: static, [https://en.wikipedia.org/wiki/Multiple_dispatch multiple dispatch], dynamic-dispatch, strongly typed, custom types (numbers with range, enums)
* multiple return values
* TODO look at haskell
* [https://en.wikipedia.org/wiki/Type_safety Type_safety]
* [https://en.wikipedia.org/wiki/Liskov_substitution_principle Liskov_substitution_principle]
* [https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic]
* [https://en.wikipedia.org/wiki/Computer_architecture Computer architecture]s: [https://en.wikipedia.org/wiki/Reconfigurable_computing Reconfigurable computing]
* [https://en.wikipedia.org/wiki/Software Software][https://en.wikipedia.org/wiki/Software_design  design]: [https://en.wikipedia.org/wiki/Abstraction_(computer_science Abstraction], [https://en.wikipedia.org/wiki/Modularity Modularity], [https://en.wikipedia.org/wiki/Information Information][https://en.wikipedia.org/wiki/Information_hiding  hiding], [https://en.wikipedia.org/wiki/Fault_tolerance Fault tolerance], [https://en.wikipedia.org/wiki/Extensibility Extensibility], [https://en.wikipedia.org/wiki/Maintainability Maintainability], [https://en.wikipedia.org/wiki/Software_durability Software_durability], [https://en.wikipedia.org/wiki/Reusability Reusability], [https://en.wikipedia.org/wiki/Usability Usability], [https://en.wikipedia.org/wiki/Performance Performance], [https://en.wikipedia.org/wiki/Software_portability Software_portability], [https://en.wikipedia.org/wiki/Scalability Scalability]
* [https://en.wikipedia.org/wiki/Programming_paradigm Programming paradi][https://en.wikipedia.org/wiki/Programming_paradigm gms]: [https://en.wikipedia.org/wiki/Imperative_programming Imperative] ([https://en.wikipedia.org/wiki/Procedural_programming Procedural], [https://en.wikipedia.org/wiki/Object-oriented_programming Object-oriented] ([https://en.wikipedia.org/wiki/Polymorphism_(computer_science) Polymorphism])) / [https://en.wikipedia.org/wiki/Declarative_programming Declarative] ([https://en.wikipedia.org/wiki/Functional_programming functional], [https://en.wikipedia.org/wiki/Logic_programming Logic_programming] (using a DSL), [https://en.wikipedia.org/wiki/Reactive_programming Reactive_programming] (using a DSL))
* [https://en.wikipedia.org/wiki/Homoiconicity https://en.wikipedia.org/wiki/Homoiconicity]
* [https://en.wikipedia.org/wiki/Dependent_type https://en.wikipedia.org/wiki/Dependent_type]?
* Graphical version? [https://en.wikipedia.org/wiki/Dataflow_programming https://en.wikipedia.org/wiki/Dataflow_programming]? (+ [https://en.wikipedia.org/wiki/Automatic_parallelization https://en.wikipedia.org/wiki/Automatic_parallelization])
* [https://en.wikipedia.org/wiki/Actor_model https://en.wikipedia.org/wiki/Actor_model]?
* Future / promise
* multiple inheritance
* atomic operations? / locking?
* Undoing / redoing instead of operation confirmation
* lexical scoping?
* lisp like exception handling (maybe multiple choices but then how should you write libraries?)
* tail call optimization
* code stored as binary data (for efficiency and storage)
* git like version control
* Look at existing vulnerabilities and find out how they could be prevented by the language
* TODO NEEDTOUNDERSTAND [https://en.wikipedia.org/wiki/Synchronous_programming_language https://en.wikipedia.org/wiki/Synchronous_programming_language]
* self-hosting compiler
* REPL
* lisp-1!!! vs lisp-2
* call by…. Sharing?
* Expression-language
* [https://en.wikipedia.org/wiki/Erlang_(programming_language) https://en.wikipedia.org/wiki/Erlang_(programming_language)]
* testing
* hot swapping
* [https://en.wikipedia.org/wiki/Mixins https://en.wikipedia.org/wiki/Mixins] ?
* [https://en.wikipedia.org/wiki/Go_(programming_language) https://en.wikipedia.org/wiki/Go_(programming_language)]
* Null?
* Gui
* tabs for indentiation, spaces for alignment / auto format
* [https://en.wikipedia.org/wiki/Definite_assignment_analysis https://en.wikipedia.org/wiki/Definite_assignment_analysis]
* object database
* distributed objects/calls
* .green
* If using semver enforce it using compiler
* Good IDE, debugger, profiler, optimizer
* common lisp object system (prevents design patterns)
* permission system
* code-completion
* common lisp, scheme
* cooperative multitaksing? (I dont think so)
* time travelling debugger [https://elm-lang.org/news/time-travel-made-easy https://elm-lang.org/news/time-travel-made-easy]
* defense in depth (running applications in container like things?)
* everything should be machine-readable (contracts, rules, wikis, etc.)
* [https://en.wikipedia.org/wiki/Clojure https://en.wikipedia.org/wiki/Clojure] immutability
* [https://en.wikipedia.org/wiki/Exception_handling https://en.wikipedia.org/wiki/Exception_handling]? [https://en.wikipedia.org/wiki/Software_transactional_memory https://en.wikipedia.org/wiki/Software_transactional_memory] ?
* locality of data (for speedup?) [https://en.wikipedia.org/wiki/Partitioned_global_address_space https://en.wikipedia.org/wiki/Partitioned_global_address_space]?
* https://crystal-lang.org/reference/guides/writing_shards.html
* https://en.wikipedia.org/wiki/Language-based_security
* Rust
* Trust layers (you need to trust all lower layers)

Interesting: [http://appinventor.mit.edu/ http://appinventor.mit.edu/]

# Replacing (programming languages and software)

[https://en.wikipedia.org/wiki/List_of_programming_languages List_of_programming_languages]* [https://en.wikipedia.org/wiki/A_Sharp_(.NET A_Sharp_(.NET][https://en.wikipedia.org/wiki/A_Sharp_(.NET) )] → [https://en.wikipedia.org/wiki/Ada_(programming_language) Ada]
** +structured, +statically typed, +imperative, +object-oriented, +strong typing
* [https://en.wikipedia.org/wiki/AMPL https://en.wikipedia.org/wiki/AMPL] replacing as DSL
* [https://en.wikipedia.org/wiki/Assembly_language https://en.wikipedia.org/wiki/Assembly_language] (using DSL)
* Scripting languages? 
* TODO graphic programming (OpenCL, OpenGL, Vulkan, ...)

# Working for</div>
* Desktop computers: yes
* Embedded
** Robots (4KB RAM): yes (reduced)
