; Functions to ask questions
(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))


; Classes
(defclass PROGRAMMER (is-a USER)
	(role concrete)
	(slot experience)
    (slot career)
	(slot has_project))

(defclass PROJECT (is-a USER)
	(role concrete)
	(slot platform)
    (slot scope)
	(slot high_performance))

(defclass LANGUAGE (is-a USER)
	(role concrete)
	(slot lname)
	(slot career)
	(slot type)
	(slot body))

; Class instances
(definstances PROGRAMMER
	(user of PROGRAMMER 
	(has_project yes))
)

(definstances PROJECT
	(project of PROJECT 
	(scope frontend))
)

(definstances LANGUAGE
	(language of PROJECT (name Python))
)

; Query rules
; Various questions are asked to the user. Several questions depend on answers to previous questions
(defrule new-programmer ""
	(not (new ?))
    (not (language ?))
    =>
    (assert (new (yes-or-no-p "Are you new to programming? (yes/no)"))))

(defrule motivation ""
	(new yes)
	(not (motivation ?))
    (not (language ?))
    =>
    (assert (motivation (ask-question "Are you career oriented or personally interested? (career/interested)"
	career interested))))

(defrule project-in-mind ""
	(not (project-in-mind ?))
    (not (language ?))
    =>
    (assert (project-in-mind (yes-or-no-p "Do you have a project in mind? (yes/no)"))))

(defrule learning-style ""
	(project-in-mind no)
	(new yes)
	(not (learning-style ?))
    (not (language ?))
    =>
    (assert (learning-style (ask-question "Do you want to learn the easy way, or the harder (but easier to pick up other languages later) way? (easy/hard)"
	easy hard))))

(defrule interest ""
	(motivation interested)
	(project-in-mind no)
	(new no)
	(not (interest ?))
    (not (language ?))
    =>
    (assert (interest (ask-question "Answer with the topic you are most interested in (machine-learning/game-design/mobile-development/other"
	machine-learning game-design mobile-development other))))

(defrule unique
	(interest other)
	(not (somethingnew ?))
	(not (language ?))
	=>
	(assert (unique (yes-or-no-p "Do you want a unique challenge? (yes/no)"))))

(defrule platform ""
	(project-in-mind yes)
	(not (platform ?))
    (not (language ?))
    =>
    (assert (platform (ask-question "Which domain/platform is your project? (gaming/web/mobile/machine-learning)"
	gaming web mobile machine-learning))))
	
(defrule industry ""
	(motivation career)
	(not (industry ?))
    (not (language ?))
    =>
    (assert (industry (ask-question "Which term best describes the job you are looking for? (bigcompany/web/gaming/money/startup)"
	bigcompany web gaming money startup))))

(defrule company ""
	(motivation career)
	(not (company ?))
    (not (language ?))
    =>
    (assert (company (ask-question "What big tech company are you leaning towards? (google/microsoft/apple/facebook/any)"
	google/microsoft/apple/facebook/any))))


; Language suggestion rules
; Using the responses these rules select a language to be suggested
(defrule c ""
	(learning-style hard)
	(new yes)
	(project-in-mind no)
    (not (language ?))
    =>
    (assert (language "C/C++. If you like the idea of bare basic fundamentals, pick C otherwise C++ for more features.")))

(defrule cplusplus ""
	(or(industry gaming)(platform gaming))
    (not (language ?))
    =>
    (assert (language "C++. It is commonly used in game development.")))

(defrule python ""
	(or
		(and(new yes)(learning-style easy))
		(company google)
		(company facebook)
		(industry startup)
		(platform machine-learning))
    (not (language ?))
    =>
    (assert (language "Python.")))

(defrule csharp ""
	(company microsoft)
    (not (language ?))
    =>
    (assert (language "C#.")))

(defrule swift ""
	(company apple)
    (not (language ?))
    =>
    (assert (language "Swift.")))

(defrule java ""
	(or(industry money)(company any)(industry mobile)(platform mobile))
    (not (language ?))
    =>
    (assert (language "Java.")))

(defrule javascript ""
	(or(platform web)(industry web))
    (not (language ?))
    =>
    (assert (language "Javascript. ")))

(defrule haskell ""
	(unique yes)
    (not (language ?))
    =>
    (assert (language "Haskell.")))

(defrule defaultlanguage ""
  (declare (salience -10))
  (not (language ?))
  =>
  (assert (language "Some answers didn't add up, so I'm just going to default to Python.")))

; Startup and Conclusion rules

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "The Programming Language Suggestor")
  (printout t crlf crlf))

(defrule print-language ""
  (declare (salience 10))
  (language ?item)
  =>
  (printout t crlf crlf)
  (printout t "Suggested Programming Language:")
  (printout t crlf crlf)
  (format t " %s%n%n%n" ?item))

