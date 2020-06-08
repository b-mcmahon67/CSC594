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
   (?u familial ?x)
   (?o familial ?x)
   (not (familial-check ?x))
   )
  -->
  (
   (add (familial-check ?x)) ; Don't repeat this preference
   (add (family ?u ?o)) ; Are related to one another
   (add (family ?o ?u)) 
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
  ;;; Sometimes people share qualities
(defrule SimilarQualsToYou-B 55 ; From the B cycle, fire this one first.
  (
   (user ?u)
   (other-user ?o)
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
 
;;; Sometimes people share multiple characteristics
(defrule similarPersonalities-B 55 ; From the B cycle, fire this second first.
  (
   (user ?u)
   (other-user ?o)
	(and
		(?o similar ?u)
		(?u similar ?o)
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


;;; Sometimes people are dissimilar in some ways
(defrule DisSimilarToYou-B 50 ; From the B cycle, fire this one third
  ((user ?u)
   (other-user ?o)
   (?u prefers ?x)
   (not (?o prefers ?x))
   (not (pref-dis-check ?x)))
  -->
  ((add (pref-dis-check ?x)) ; Don't repeat this preference
   (add (dissimilar ?u ?o)) ; Are similar to one another
   (add (dissimilar ?o ?u))
   (add (disagreement ?u ?o ?x))
   (print (?u has the preference ?x that ?o does not))))

;;; If one or the other wants a long-term relationship and their opposite wants a short-term relationship
;;; then they are not compatible.
(defrule NotCompatible-C1 45 ; From the C cylce, fire this one first.
  (check-not-compatible
   (other-user ?o)
   (user ?u)
   (or
    (and
     (?o prefers short-term-relationship)
     (?u prefers long-term-relationship))
    (and
     (?o prefers long-term-relationship)
     (?u prefers short-term-relationship))))
  -->
  ((print "When one prefers long-term and the other prefers short-term you aren't compatible")
   ))

;;; If one wants a long-term relationship and the other wants a short-term relationship
;;; the one who wants a long-term relationship will tend to get hurt
(defrule NotCompatible-C 40 ; From the C cylce, fire this one second.
  (check-not-compatible
   (other-user ?o)
   (user ?u)
   (?o prefers long-term-relationship)
   (?u prefers short-term-relationship))
  -->
  ((remove check-not-compatible)
   (add (hurt ?u ?o))
   (add (disagreement ?u ?o short-or-long-term))
   (print (Because ?o wants long term but ?u wants short term the relationship will probably hurt ?o))
   ))

;;; People who get hurt may get angry
(defrule HurtGetAngry-D 35
  ((hurt ?x ?y)
   )
  -->
  ((remove (hurt ?x ?y))
   (add disagreement ?x ?y hurt)
   (add (angry-at ?y ?x))
   (print (Because ?y is hurt... ?y might get angry at ?x))))

;;; It is not a good idea to get into a romance with someone who will be angry at you
;;; Plus it is not nice to make them unhappy.
(defrule LoveAdvice-D 33
  ((user ?u)
   (angry-at ?o ?u))
  -->
  ((print (Hey ?u))
   (ask "I have some advice for you. Would you like to hear it?"
       ((print (?u you should not form a relationship with ?o))
	(print (Why... because if you do ?o might get angry at you))
	(print (And angry people are unpleasant to be around. Plus why make ?o unhappy...)))
       ((print "OK. Sorry. It was good advice.")
	(add (no-accept-advice ?u about ?o))))
   (add (disagreement ?o ?u angry-about-short-term))
   (add tell-story)
   (add form-agreement)
   (remove (angry-at ?o ?u))))

;;; People who don't accept advice about romance have trouble working out problems.
(defrule NoAdvice-D 31
  ((user ?u)
   (no-accept-advice ?u about ?o))
  -->
  ((remove (no-accept-advice ?u about ?o))
   (print (My advice to ?o is to avoid a relationship with ?u because...))
   (print (...People like ?u who do not accept advice about romance cannot deal with problems and never change.))))

(defrule FormAgreement-E1 12
  (form-agreement
   (user ?u)
   (other-user ?o)
   (not (no-accept-advice ?u about ?o))
   (or
    (disagreement ?u ?o ?z)
    (disagreement ?o ?u ?z)))
  -->
  ((remove form-agreement)
   (add form-agreement2)
   (print "Of course even though you are not compatible. If the two of you still wanted a relationship...")
   (print "Then consider...")))

(defrule FormAgreement-E2 10
  (form-agreement2
   (disagreement ?u ?o ?z))
  -->
  ((print (?u and ?o should try to reach some agreement about their difference ?z))
   (remove (disagreement ?u ?o ?z))))

(defrule TellStory-F2 2 ; Very low priority. Should come last.
  (tell-story)
  -->
  ((remove tell-story)
   (tell-the-story)))



;;; Our LISP functions that get called on the RHS of rules:
;;; Note that we can also build rules dynamically while the system is running.

(defun gather-description ()
  (let ((user nil)(prefs nil)(self nil)
	(prefx '(beauty young healthy refined smart creative kind generous thoughtful nice short tall strong))
	(aversx '(bad-health old short tall smart uneducated mean coarse thrifty no-humor)))
    
    (format t "First, what is your name?~%")
    (setf user (read))
    (format t "Thanks ~s, now we'll get some information from you.~%~%" user)
    (format t "Enter tokens from each of the following lists that apply to your tastes. Then terminate~%")
    (format t "your data entry with a space then the slash character: / ~%~%")
    (format t "Preferences: ~s~%" prefx)
    (setf prefs (read-delimited-list #\/))
    (format t "Aversions: ~s~%" aversx)
    (setf avers (read-delimited-list #\/))
    (format t "Which of the above tokens apply to you?~%")
    (setf quals (read-delimited-list #\/))

    (mapcar
     #'(lambda (pref)
	 (setf temp (list user 'prefers pref))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     prefs)

    (mapcar
     #'(lambda (aver)
	 (setf temp (list user 'has-aversion aver))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     avers)

    (mapcar
     #'(lambda (qual)
	 (setf temp (list user 'has-quality qual))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     quals)
    *wm*
    (add-wm (list 'user user))
    )
  )






(defun build-user ()
  (let (
	(temp nil)
	(user nil)
	(familial '(john joe bob lisa))
	(avers '(bad-health short uneducated no-humor))
	(quals '(tall smart nice generous)))
    (format t "OK. I'll build your profile. What is your first name?~%")
    (setf user (read))
    (add-wm (list 'user user))
    (mapcar
     #'(lambda (familial)
	 (setf temp (list user 'family familial))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     prefs)

    (mapcar
     #'(lambda (aver)
	 (setf temp (list user 'has-aversion aver))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     avers)

    (mapcar
     #'(lambda (qual)
	 (setf temp (list user 'has-quality qual))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     quals)
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
