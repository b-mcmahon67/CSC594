#|

2020-04-25: Clark Elliott
2020-06-7: Brian McMahon

Brian McMahon edit for CSC 594 Content Theory based on 
Romance.lisp created by Clark Elliott for CSC594

Original Idea:

I borrowed a few simple ideas from the shared Human Romantic Love start on a content theory we created, and
place these common-sense ideas into a few rules. No claims of importance or social correctness!

What sort of interaction are two people likely to have?

What differences will they have?

What compatibilities?

What bad things can happen in love?

If we wanted to, we could extend this to a dating / advice application where through socket interface
users enter their preferences.

I have added paraphrased one single story from the shared document that is indexed by a key token. We could
collect hundreds (along with many indexes each) and deliver them at appropriate junctures in the interaction.

Note: If you extend this, keep in mind that inside quotes, lisp is forgiving, but inside of lists
then commas, quotes, periods and question markes have programmatic value.

BM Idea:

Adapt the original Romance.lisp program for acute stress to try and create matching patterns for agents to address if a situation will be stressful to them


|#

(setf *wm* '(empty start))
(setf *rules* nil)
(defun r () (load "stress")) ; Easy to load this file acutestress.lisp: (r)

(defrule ask-for-sim-event 3
  (start)
  -->
  (
   (remove start)
   (ask "Do want to run a simulation event?"
	((print "Running event now...")
	 (ask "You can create your own event manually. But I can select an event for you (recommended)?"
	      ((build-user))
	      ((add gather-data)))
	 (add load-data))
	((print "Run an event again if you would like!")))))

(defrule self-description 20
  (
   self-description
   )
  -->
  ((remove self-description)
   (gather-description)))


(defrule KillGatherData 5 ; low priority, goes last
  (
   gather-data
   )
  -->
  (
   (remove gather-data)))

(defrule StopLoadingdata 5 ; lowest priority.
  (load-data)
  -->
  ((print "Done loading data...")
   (remove load-data)))


(defrule LoadTheData 10 ; low priority
  (load-data
   )
  -->
  ((print "Loading data...")
   (add check-not-compatible)
   (add create-other-user)))



;;; Individuals related to you
(defrule familialRelation-B 55 ; From the B cycle, fire this one first.
  (
   (user ?u)
   (other-user ?o)
   (?u family ?x)
   (?o family ?x)
   (not (familial-check ?x))
   )
  -->
  (
   (add (familial-check ?x)) ; Don't repeat this preference
   (add (familial ?u ?o)) ; Are related to one another
   (add (familial ?o ?u)) 
   (print (?u and ?o are related... ?x))
   )
  )
  
 ;;; Social connections
(defrule socialRelation-B 55 ; From the B cycle, fire this one first.
  (
   (user ?u)
   (other-user ?o)
   (?u has-aversion ?x)
   (?o has-aversion ?x)
   (not (pref-aver ?x))
   )
  -->
  (
   (add (pref-aver ?x)) ; Don't repeat this preference
   (add (similarAversion ?u ?o)) ; Are similar to one another
   (add (similarAversion ?o ?u)) 
   (print (?u and ?o share the aversion... ?x))
   )
  )
  ;;; Story match physical harm characteristics of agent
(defrule SimilarQualsToYou-B 55 ; From the B cycle, fire this one first.
  (
   (user ?u)
   (story ?s)
   (?u has-quality ?x)
   (?o has-quality ?x)
   (not (pref-qual?x))
   )
  -->
  (
   (add (pref-qual ?x)) ; Don't repeat this preference
   (add (similarQuals ?u ?o)) ; Are similar to one another
   (add (similarQuals ?o ?u)) 
   (print (?u and ?o share the quality... ?x))
   )
  )
 
;;; Story match physical emotional characteristics of agent
(defrule similarPersonalities-B 55 ; From the B cycle, fire this second first.
  (
   (user ?u)
   (story ?s)
	(and
		(?o similarAversion ?u)
		(?u similarAversion ?o)
		(?o similarQuals ?u)
		(?u similarQuals ?o))
											)
  -->
  (
   (add (fatuation ?u ?o)) ; Are compatible to one another
   (add (fatuation ?o ?u))
   (print (?u and ?o are highly compatible together))
   )
  )
  
(defun build-user ()
  (let (
	(temp nil)
	(user nil)
	(familial '(john joe bob lisa))
	(social '(bad-health short uneducated no-humor))
	(emotharm '(x y z))
	(phsyharm '(attack steal rob))
    (format t "OK. I'll build your profile. What is your first name?~%")
    (setf user (read))
    (add-wm (list 'user user))
    (mapcar
     #'(lambda (familial)
	 (setf temp (list user 'family familial))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     familials)

    (mapcar
     #'(lambda (emotharm)
	 (setf temp (list user 'has-emotion emot))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     emots)

    (mapcar
     #'(lambda (phsyharm)
	 (setf temp (list user 'has-phsyical phys))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     physicals)
    'leslie-built
    )
  )

(setf stories
      '((situation1

       "A dog walks past you .")
(situation2

       "A robber has entered your house an is going to attack you ")
))
))
