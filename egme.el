;;Copyright (C) 2021 Category <category@[no_spam]quintendo.uk>
;;
;;This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; version 2.
;;
;;This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
;;
;;You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

(defgroup egme nil
  "Variables for customizing the Emacs Games Master Emulator."
  :group 'games)

(defcustom egme-action-list '("Abandon"
			      "Abuse"
			      "Adversity"
			      "Agree"
			      "Ambush"
			      "Antagonise"
			      "Arrive"
			      "Assist"
			      "Attach"
			      "Befriend"
			      "Bestow"
			      "Betray"
			      "Block"
			      "Break"
			      "Celebrate"
			      "Change"
			      "Communicate"
			      "Control"
			      "Create"
			      "Cruelty"
			      "Deceive"
			      "Delay"
			      "Desert"
			      "Develop"
			      "Dispute"
			      "Disrupt"
			      "Divide"
			      "Dominate"
			      "Expose"
			      "Failure"
			      "Fight"
			      "Gratify"
			      "Guide"
			      "Harm"
			      "Heal"
			      "Immitate"
			      "Imprison"
			      "Inform"
			      "Inquire"
			      "Inspect"
			      "Intolerance"
			      "Judge"
			      "Kill"
			      "Malice"
			      "Mistrust"
			      "Move"
			      "Neglect"
			      "Open"
			      "Oppose"
			      "Oppress"
			      "Passion"
			      "Persecute"
			      "Praise"
			      "Procrastinate"
			      "Propose"
			      "Punish"
			      "Pursue"
			      "Release"
			      "Return"
			      "Ruin"
			      "Separate"
			      "Spy"
			      "Starting"
			      "Stop"
			      "Take"
			      "Transform"
			      "Travel"
			      "Trick"
			      "Trust"
			      "Violate"
			      "Waste"
			      "Work")
  "List of 'Action' variables used in the random event generator."
  :type '(repeat string)
  :group 'egme)

(defcustom egme-subject-list '("A path"
			       "A project"
			       "Adversities"
			       "Advice"
			       "Allies"
			       "Ambush"
			       "Animals"
			       "Art"
			       "Attention"
			       "Balance"
			       "Bureaucracy"
			       "Business"
			       "Competition"
			       "Danger"
			       "Death"
			       "Dispute"
			       "Disruption"
			       "Dreams"
			       "Elements"
			       "Emotions"
			       "Energy"
			       "Environment"
			       "Expectations"
			       "Extravagance"
			       "Failure"
			       "Fame"
			       "Fears"
			       "Friendship"
			       "Goals"
			       "Home"
			       "Hope"
			       "Illness"
			       "Information"
			       "Inside"
			       "Intrigues"
			       "Jealousy"
			       "Joy"
			       "Leadership"
			       "Lies"
			       "Masses"
			       "Messages"
			       "Military"
			       "Nature"
			       "New ideas"
			       "Opposition"
			       "Outside"
			       "Pain"
			       "Peace"
			       "Plans"
			       "Pleasures"
			       "Portals"
			       "Possessions"
			       "Power"
			       "Prison"
			       "Randomness"
			       "Reality"
			       "Riches"
			       "Rumor"
			       "Stalemate"
			       "Status quo"
			       "Success"
			       "Suffering"
			       "Tactics"
			       "Technology"
			       "Travel"
			       "Trials"
			       "Vehicle"
			       "Victory"
			       "Weapons"
			       "Weather"
			       "Wishes"
			       "Wounds")
  "List of 'Subject' variables used in the random event generator."
  :type '(repeat string)
  :group 'egme)

(defcustom egme--random-event-threshold 20
  "Set the upper limit of questions before a random event happens. Low values mean random events happen more frequently, high values and the are more sporadic. After this many questions a random event will definitely occur."
  :type 'natnum
  :group 'egme)

(setq egme-dice-history (list))

(setq egme-current-dice nil)
(setq egme-roll-result nil)

;; Standard probability list for ido-completing-read
(setq egme-probability-list (list
			     "0  Even odds"
			     "-1  Unlikely"
			     "-2  Very Unlikely"
			     "-3  Extremely Unlikely"
			     "-4  Near Impossible"
			     "+4  Near Certain"
			     "+3  Extremely Likely"
			     "+2  Very Likely"
			     "+1  Likely"))

(setq egme-random-counter 0)

(setq egme--random-event-list (list
			      "Remote event"
			      "NPC action"
			      "New NPC appears"
			      "Move towards thread"
			      "Move away from thread"
			      "PC positive"
			      "PC negative"
			      "NPC positive"
			      "NPC negative"
			      "Ambiguous event"))

(setq egme-npc-list (list))
(setq egme-thread-list (list))

(defun egme--random-list-item (list-to-pick-from)
  "This function takes a list as an argument, and returns a random element from within that list.

Will return nil if provided list is nil."

  (cond
    (list-to-pick-from (nth (random (length list-to-pick-from)) list-to-pick-from))
    (t nil)))

  (defun egme--get-dice ()
    "Get the required dice-roll from user input on the mini-buffer. Dice rolls to be expected in the usual [number]D[dice-type][modifier] format used by RPGs, for example '2D6' for 2 six-sided dice, or '3d8+2' for 3 eight-sided dice, with 2 added to the result. If the format is given without number (for example 'd100'), then it is assume to be a single dice being rolled.

If no input is given, then it will return the last dice rolled. A full history of rolls is stored in 'egme-dice-history', accessible via the arrow keys when asked for input.

Returns the dice-type, which is also stored in the variable egme-current-dice - returns nil if input can't be parsed into a dice roll."

    (setq egme-current-dice (read-string (format "Enter dice roll (default %s): " (car egme-dice-history))
					 nil
					 'egme-dice-history
					 (car egme-dice-history)))
  
    ;; Add a leading "1" in case user inputs without type (i.e just "D100")
    (when (string-match "^[dD]" egme-current-dice)
      (setq egme-current-dice (concat "1" egme-current-dice)))
  
    ;; Look for string in dice-roll format, return if found
    (when (string-match "[1-9][0-9]?[dD][1-9][0-9]*\\([+-][0-9]+\\)?" egme-current-dice)
      (setq egme-current-dice (match-string 0 egme-current-dice))))

(defun egme--calculate-dice (&optional dice-roll)
  "Calculates the current dice roll. If called alone, roll the variable egme-current-dice. If argument DICE-ROLL is provided, roll that - it must be in RPG dice notation ('1d20', '3d10+8', '2d6-4', etc). Return the result of the dice roll, and store in the variable egme-roll-result.

Current roll is broken down into the following variable for calculating:-
 +egme-current-dice-quantity
 +egme-current-dice-type
 +egme-current-dice-modifier

This function loops for the quantity of dice, summing up random numbers for the appropriate type, then applying the modifier. In the case of a multiple D6 type (ie D66/D666/D6666...) then instead of summing the results it treats each D6 roll as a different digit in the final result."

  ;; Reset last roll result
  (setq egme-roll-result 0)
  (setq egme-multi-6-temp nil)

  ;; Set egme-current-dice if an option was passed with the function call
  (when dice-roll
    (setq egme-current-dice dice-roll))
  
  ;; Get quantity of dice rolled
  (string-match "^[1-9]+" egme-current-dice)

  (setq egme-current-dice-quantity (string-to-number (match-string 0 egme-current-dice)))

  ;; Get current dice type
  (string-match "[dD][1-9][0-9]*" egme-current-dice)

  (setq egme-current-dice-type (string-to-number (string-trim-left (match-string 0 egme-current-dice) "[dD]")))

  ;; Get modifier (if present, else set to 0)
  (if (string-match "[+-][0-9]+$" egme-current-dice)
      (setq egme-current-dice-modifier (string-to-number (match-string 0 egme-current-dice)))
    (setq egme-current-dice-modifier 0))
  
  ;; Check if dice type is a D66/D666/D6666 etc
  (if (string-match "^66+$" (number-to-string egme-current-dice-type))
      ;; If a multi-6 dice, roll each D6 and combine as string, then repeat for each quantity of rolls
      (dotimes (n egme-current-dice-quantity)
	      (dotimes (n (length (number-to-string egme-current-dice-type)))
          (setq egme-multi-6-temp (concat egme-multi-6-temp (number-to-string (+ 1 (random 6))))))
	      (setq egme-roll-result (string-to-number egme-multi-6-temp)))
    ;; Else calculate dice as usual
    (dotimes (n egme-current-dice-quantity)
      (setq egme-roll-result (+ egme-roll-result (+ 1 (random egme-current-dice-type))))))

  ;; Add the modifier to the result, for the final roll
  (setq egme-roll-result (+ egme-roll-result egme-current-dice-modifier)))

(defun egme--print-output (print-string)
  "This function takes a string in as an argument, and prints it's output into the current buffer, within a GameMaster org block.

If the current buffer is not an org-mode document, it will check if it is fundamental. If it is, org-mode will be activated, else it will throw an error stating which major-mode is currently active. It will still print output regardless."

  ;; Move point to "safe" position
  (end-of-line)

  ;; Add additional newline if current line contains is non-blank
  (when (string-match "[:ascii:]" (thing-at-point 'line t))
      (newline))

  (newline)

  ;; Check mode, change to org if non-fundamental, else throw message
  (if (not (string-match "^org" (format "%s" major-mode)))
      (if (string-match "^fundamental" (format "%s" major-mode))
	  (org-mode)
	(message "Current buffer is neither org nor fundamental, leaving as-is")))

  ;; Output the start line
  (insert "#+BEGIN_GameMaster")
  
  (newline)

  ;; Output text generated by egme functions
  (insert print-string)

  (newline)

  ;; Output the end line
  (insert "#+END_GameMaster")
  
  (newline 2)

  t)

    (defun egme--random-event ()
      "A function for genereating unexpected events.

  When an oracle question is asked, this function is called. It keeps a counter in the variable egme-random-counter, which is incremented each time this is called. A random number is generated between 0 and the variable egme--random-event-threshold - if the result is lower than the current egme-random-counter value, then a random event is generated. A focus, action and subject are randomly selected from the lists (egme--random-event-list, egme-action-list, and egme-subject-list respectively). If a random event was generated, the counter is reset to 0.

If the chosen event concerns an NPC (ignoring the New NPC event), it will display a random NPC from the current list (if available). Likewise, if the event concerns a thread, it will pick a random one from the list.

This function then returns the random event text, for the calling function to pass on to for user output."

      ;; Increment random counter
      (setq egme-random-counter (1+ egme-random-counter))

      ;; Clear random event output text
      (setq egme--random-event-output nil)

      ;; Generate a random number up to the egme--random-event-threshold, and compare against current counter
      (if (< (random egme--random-event-threshold) egme-random-counter)

          ;; Below batch of steps to take if 
          (progn
            ;; Announce the event
            (setq egme--random-event-output "\n------------\nRandom Event!")

            ;; Pick random event from the random event focus list
            (setq egme--random-event-output (concat egme--random-event-output (format "\n      Focus:  %s" (egme--random-list-item egme--random-event-list))))

            ;; Check if it's an NPC event
            (if (string-match-p "NPC" egme--random-event-output)
		;; Make sure it is /not/ a "New NPC" event
		(if (not (string-match-p "New NPC" egme--random-event-output))
		    ;; Only change output if NPC list is non-nil
		    (if (egme--parse-npc-list)
			(setq egme--random-event-output (concat egme--random-event-output (format "\n        NPC:  %s" (egme--random-list-item (egme--parse-npc-list))))))))

            ;; Check if it's a Thread event, add a random Thread from the list - just checks if "thread" is in the current print output variable
            (if (string-match-p "thread" egme--random-event-output)
                ;; Only change output if Thread list is non-nil
                (if (egme--parse-thread-list)
                    (setq egme--random-event-output (concat egme--random-event-output (format "\n     Thread:  %s" (egme--random-list-item (egme--parse-thread-list)))))))

            ;; Add event details
            (setq egme--random-event-output (concat egme--random-event-output (format "\n     Detail:  %s" (egme--random-list-item egme-action-list))(format " / %s" (egme--random-list-item egme-subject-list))))

            ;; Reset the random counter
            (setq egme-random-counter 0)

            ;; Return text output
            egme--random-event-output)

        ;; Return nil if no event found
        nil))

(defun egme--open-org-drawer ()
  "This function will open an org-mode drawer on the current line, if it is currently closed.

Open state is determined by checking if current line is a drawer, and if the text at the end of the line is visible. If it is invisible, open the drawer with org-cycle."

  (interactive)

  (if (and (org-at-drawer-p) (invisible-p (point-at-eol)))
      (org-cycle)
    (message "No closed drawer to open.")))

(defun egme--close-org-drawer ()
  "This function will close an org-mode drawer on the current line, if it is currently open.

Open state is determined by checking if current line is a drawer, and if the text at the end of the line is visible. If it is not invisible, close the drawer with org-cycle."

  (interactive)
  
  (if (and (org-at-drawer-p) (not (invisible-p (point-at-eol))))
      (org-cycle)
    (message "No open drawer to close")))

(defun egme-roll-dice ()
  "This function is for a user to generate the results from a dice roll, and output them into the current buffer.

egme--get-dice is called to get user input, egme-calculate dice is used to generate the result, and egme--print-output is used to place this into the current buffer, creating new lines below the point.

This function is interactively callable via M-x, and a prime input option for key-binding."
  ; Let user call via M-x
  (interactive)

  ; Get dice size from user
  (egme--get-dice)

  ; Check dice input was correct
  (if egme-current-dice
    ; If valid then calculate result
    (egme--calculate-dice)
    ; Else drop an error message and exit
    (user-error "Could not parse dice roll"))

  ; Print results
  (egme--print-output (concat (format "Rolled:  %s" egme-current-dice) (format "\nResult:  %s" egme-roll-result)))

  ;; Update dashboard
  (egme--update-display-buffer)
  
  egme-roll-result)

  (defun egme-y-n-oracle ()
    "The basic oracle function. This will provide Yes/No answers to questions posed to the games master, and outputs the results in the current buffer in the standard games master format.

The user will be asked to input a question - if the end of the current line is parsed as a question, then that will be set as the initial user input. If a quesiton is provided, it will be printed along with the results.
Next, the user will be asked for the likelihood of this result. These options are stored in the list egme-probability-list, and selected via ido-completing-read. Each option is a modifier between -4 and +4, along with a basic description of the probability. This basic description will be printed along with the results.
The answer is generated by rolling 1D10 and applying the chosen modifier, any result of a 6+ will be a 'Yes', anything else a 'No'. A D6 is also rolled, to see if it is an extreme answer - on a 1 it is a minor result (', but...'), and on a 2 it is a major result (', and...').

The function egme--random-event is also called to see if anything unexpected occurs - any change will be added to the variable egme-oracle-output before it gets passed on for user output."
    (interactive)

    ; Reset some variables
    (setq egme-oracle-ouput nil)
    (setq egme-oracle-answer nil)
    (setq egme-current-question nil)
    
    ; Check if the current line contains a question (ends in a question mark, and gets everything from the last ellipses to the end of the line)
    (setq egme-current-line (thing-at-point 'line t))
    (if (string-match "\\.?[0-9A-Za-z ,:;']+\\? *$" egme-current-line)
      ; If that current line is a question, strip any leading ellipses or spaces, then set as pre-filled input when asking for the current question
      (setq egme-current-question (read-string "What is the question? " (replace-regexp-in-string " *$" "" (replace-regexp-in-string "^\\.* *" "" (match-string 0 egme-current-line)))))
      ; Else just ask user for question
      (setq egme-current-question (read-string "What is the question?: ")))
    
    ; Get probability from the user
    (setq egme-current-probability-choice (ido-completing-read "Probability modifier: " egme-probability-list))

    (string-match "[+\-]?[0-9]" egme-current-probability-choice)
    (setq egme-current-probability-modifier (match-string 0 egme-current-probability-choice))
    
    ; Roll dice, apply modifier
    (setq egme-oracle-answer-roll (+ (egme--calculate-dice "1d10") (string-to-number egme-current-probability-modifier)))
    (setq egme-oracle-answer-modifier (egme--calculate-dice "1d6"))

    ; Convert dice rolls into result text - check if modified oracle roll is 6+ ('Yes')
    (if (>= egme-oracle-answer-roll 6)
      ; If greater, then answer yes
      (setq egme-oracle-answer "Yes")
      ; Else answer no
      (setq egme-oracle-answer "No"))
      
    ; Apply answer modifier (if applicable)
    ; Add 'but' if rolled 1, add 'and' if rolled 2
    (cond ((eq egme-oracle-answer-modifier 1) (setq egme-oracle-answer (concat (format "%s" egme-oracle-answer) ", but...")))
          ((eq egme-oracle-answer-modifier 2) (setq egme-oracle-answer (concat (format "%s" egme-oracle-answer) ", and..."))))      


    ;; Prepare output for printing
    ; Check if a question was input...
    (if (> (length egme-current-question) 0)
      ; ..then add quesiton to the output with results
      (setq egme-oracle-output (format "   Question:  %s\n" egme-current-question))
      (setq egme-oracle-output ""))

    ; Get probability text
    (string-match "[A-Za-z][A-Za-z ]*" egme-current-probability-choice)
    (setq egme-probability-text (match-string 0 egme-current-probability-choice))

    ; Add probability and results to output
    (setq egme-oracle-output (concat egme-oracle-output (format "Probability:  %s\n------------" egme-probability-text) (format "\n     Answer:  %s" egme-oracle-answer)))

    ; Check for Random events, add any text to output
    (setq egme-oracle-output (concat egme-oracle-output (egme--random-event)))

    ; Send output string to display to user 
    (egme--print-output egme-oracle-output))

(defun egme-actions-oracle ()
  (interactive)

  "This function will randomly generate a pair of action words, for interpreting against the current scene/NPC etc"

  (let* ((action-1-list (list
                         "Abandon"
                         "Accompany"
                         "Activate"
                         "Agree"
                         "Ambush"
                         "Arrive"
                         "Assist"
                         "Attack"
                         "Attain"
                         "Bargain"
                         "Befriend"
                         "Bestow"
                         "Betray"
                         "Block"
                         "Break"
                         "Carry"
                         "Celebrate"
                         "Change"
                         "Close"
                         "Combine"
                         "Communicate"
                         "Conceal"
                         "Continue"
                         "Control"
                         "Create"
                         "Deceive"
                         "Decrease"
                         "Defend"
                         "Delay"
                         "Deny"
                         "Depart"
                         "Deposit"
                         "Destroy"
                         "Dispute"
                         "Disrupt"
                         "Distrust"
                         "Divide"
                         "Drop"
                         "Easy"
                         "Energize"
                         "Escape"
                         "Expose"
                         "Fail"
                         "Fight"
                         "Flee"
                         "Free"
                         "Guide"
                         "Harm"
                         "Heal"
                         "Hinder"
                         "Imitate"
                         "Imprison"
                         "Increase"
                         "Indulge"
                         "Inform"
                         "Inquire"
                         "Inspect"
                         "Invade"
                         "Leave"
                         "Lure"
                         "Misuse"
                         "Move"
                         "Neglect"
                         "Observe"
                         "Open"
                         "Oppose"
                         "Overthrow"
                         "Praise"
                         "Proceed"
                         "Protect"
                         "Punish"
                         "Pursue"
                         "Recruit"
                         "Refuse"
                         "Release"
                         "Relinquish"
                         "Repair"
                         "Repulse"
                         "Return"
                         "Reward"
                         "Ruin"
                         "Separate"
                         "Start"
                         "Stop"
                         "Strange"
                         "Struggle"
                         "Succeed"
                         "Support"
                         "Suppress"
                         "Take"
                         "Threaten"
                         "Transform"
                         "Trap"
                         "Travel"
                         "Triumph"
                         "Truce"
                         "Trust"
                         "Use"
                         "Usurp"
                         "Waste"))
         (action1 (egme--random-list-item action-1-list))

         (action-2-list (list
                         "Advantage"
                         "Adversity"
                         "Agreement"
                         "Animal"
                         "Attention"
                         "Balance"
                         "Battle"
                         "Benefits"
                         "Building"
                         "Burden"
                         "Bureaucracy"
                         "Business"
                         "Chaos"
                         "Comfort"
                         "Completion"
                         "Conflict"
                         "Cooperation"
                         "Danger"
                         "Defense"
                         "Depletion"
                         "Disadvantage"
                         "Distraction"
                         "Elements"
                         "Emotion"
                         "Enemy"
                         "Energy"
                         "Environment"
                         "Expectation"
                         "Exterior"
                         "Extravagance"
                         "Failure"
                         "Fame"
                         "Fear"
                         "Freedom"
                         "Friend"
                         "Goal"
                         "Group"
                         "Health"
                         "Hindrance"
                         "Home"
                         "Hope"
                         "Idea"
                         "Illness"
                         "Illusion"
                         "Individual"
                         "Information"
                         "Innocent"
                         "Intellect"
                         "Interior"
                         "Investment"
                         "Leadership"
                         "Legal"
                         "Location"
                         "Military"
                         "Misfortune"
                         "Mundane"
                         "Nature"
                         "Needs"
                         "News"
                         "Normal"
                         "Object"
                         "Obscurity"
                         "Official"
                         "Opposition"
                         "Outside"
                         "Pain"
                         "Path"
                         "Peace"
                         "People"
                         "Personal"
                         "Physical"
                         "Plot"
                         "Portal"
                         "Possessions"
                         "Poverty"
                         "Power"
                         "Prison"
                         "Project"
                         "Protection"
                         "Reassurance"
                         "Representative"
                         "Riches"
                         "Safety"
                         "Strength"
                         "Success"
                         "Suffering"
                         "Surprise"
                         "Tactic"
                         "Technology"
                         "Tension"
                         "Time"
                         "Trial"
                         "Value"
                         "Vehicle"
                         "Victory"
                         "Vulnerability"
                         "Weapon"
                         "Weather"
                         "Work"
                         "Wound"))
         (action2 (egme--random-list-item action-2-list)))

    (egme--print-output (format "Actions: %s / %s" action1 action2))))

(defun egme-descriptions-oracle ()
  (interactive)

  "This function will randomly generate a pair of description words, for interpreting against the current scene/NPC/item etc"

  (let* ((descriptor-1-list (list
                             "Adventurously"
                             "Aggressively"
                             "Anxiously"
                             "Awkwardly"
                             "Beautifully"
                             "Bleakly"
                             "Boldly"
                             "Bravely"
                             "Busily"
                             "Calmly"
                             "Carefully"
                             "Carelessly"
                             "Cautiously"
                             "Ceaselessly"
                             "Cheerfully"
                             "Combatively"
                             "Coolly"
                             "Crazily"
                             "Curiously"
                             "Dangerously"
                             "Defiantly"
                             "Deliberately"
                             "Delicately"
                             "Delightfully"
                             "Dimly"
                             "Efficiently"
                             "Emotionally"
                             "Energetically"
                             "Enormously"
                             "Enthusiastically"
                             "Excitedly"
                             "Fearfully"
                             "Ferociously"
                             "Fiercely"
                             "Foolishly"
                             "Fortunately"
                             "Frantically"
                             "Freely"
                             "Frighteningly"
                             "Fully"
                             "Generously"
                             "Gently"
                             "Gladly"
                             "Gracefully"
                             "Gratefully"
                             "Happily"
                             "Hastily"
                             "Healthily"
                             "Helpfully"
                             "Helplessly"
                             "Hopelessly"
                             "Innocently"
                             "Intensely"
                             "Interestingly"
                             "Irritatingly"
                             "Joyfully"
                             "Kindly"
                             "Lazily"
                             "Lightly"
                             "Loosely"
                             "Loudly"
                             "Lovingly"
                             "Loyally"
                             "Majestically"
                             "Meaningfully"
                             "Mechanically"
                             "Mildly"
                             "Miserably"
                             "Mockingly"
                             "Mysteriously"
                             "Naturally"
                             "Neatly"
                             "Nicely"
                             "Oddly"
                             "Offensively"
                             "Officially"
                             "Partially"
                             "Passively"
                             "Peacefully"
                             "Perfectly"
                             "Playfully"
                             "Politely"
                             "Positively"
                             "Powerfully"
                             "Quaintly"
                             "Quarrelsomely"
                             "Quietly"
                             "Roughly"
                             "Rudely"
                             "Ruthlessly"
                             "Slowly"
                             "Softly"
                             "Strangely"
                             "Swiftly"
                             "Threateningly"
                             "Timidly"
                             "Very"
                             "Violently"
                             "Wildly"
                             "Yieldingly"))
         (descriptor1 (egme--random-list-item descriptor-1-list))

         (descriptor-2-list (list
                             "Abnormal"
                             "Amusing"
                             "Artificial"
                             "Average"
                             "Beautiful"
                             "Bizarre"
                             "Boring"
                             "Bright"
                             "Broken"
                             "Clean"
                             "Cold"
                             "Colorful"
                             "Colorless"
                             "Comforting"
                             "Creepy"
                             "Cute"
                             "Damaged"
                             "Dark"
                             "Defeated"
                             "Dirty"
                             "Disagreeable"
                             "Dry"
                             "Dull"
                             "Empty"
                             "Enormous"
                             "Extraordinary"
                             "Extravagant"
                             "Faded"
                             "Familiar"
                             "Fancy"
                             "Feeble"
                             "Festive"
                             "Flawless"
                             "Forlorn"
                             "Fragile"
                             "Fragrant"
                             "Fresh"
                             "Full"
                             "Glorious"
                             "Graceful"
                             "Hard"
                             "Harsh"
                             "Healthy"
                             "Heavy"
                             "Historical"
                             "Horrible"
                             "Important"
                             "Interesting"
                             "Juvenile"
                             "Lacking"
                             "Large"
                             "Lavish"
                             "Lean"
                             "Less"
                             "Lethal"
                             "Lively"
                             "Lonely"
                             "Lovely"
                             "Magnificent"
                             "Mature"
                             "Messy"
                             "Mighty"
                             "Military"
                             "Modern"
                             "Mundane"
                             "Mysterious"
                             "Natural"
                             "Normal"
                             "Odd"
                             "Old"
                             "Pale"
                             "Peaceful"
                             "Petite"
                             "Plain"
                             "Poor"
                             "Powerful"
                             "Protective"
                             "Quaint"
                             "Rare"
                             "Reassuring"
                             "Remarkable"
                             "Rotten"
                             "Rough"
                             "Ruined"
                             "Rustic"
                             "Scary"
                             "Shocking"
                             "Simple"
                             "Small"
                             "Smooth"
                             "Soft"
                             "Strong"
                             "Stylish"
                             "Unpleasant"
                             "Valuable"
                             "Vibrant"
                             "Warm"
                             "Watery"
                             "Weak"
                             "Young"))
         (descriptor2 (egme--random-list-item descriptor-2-list)))

    (egme--print-output (format "Descriptors: %s / %s" descriptor1 descriptor2))))

(defun egme-names-oracle ()

  "Name generator"

  (interactive)
  (let* ((name-list (list
                     "A"
                     "Action"
                     "Ah"
                     "Ahg"
                     "An"
                     "Animal"
                     "Ar"
                     "As"
                     "B"
                     "Bah"
                     "Be"
                     "Bih"
                     "Brah"
                     "Col"
                     "Color"
                     "Cor"
                     "Dah"
                     "Deeds"
                     "Del"
                     "Drah"
                     "Eee"
                     "Eh"
                     "Ei"
                     "Ell"
                     "Elements"
                     "Emotion"
                     "Ess"
                     "Est"
                     "Et"
                     "Fah"
                     "Fer"
                     "Fi"
                     "Floral"
                     "Gah"
                     "Go"
                     "Grah"
                     "Hee"
                     "Ia"
                     "Ick"
                     "In"
                     "Iss"
                     "Je"
                     "Ke"
                     "Jen"
                     "Kha"
                     "Kr"
                     "Lah"
                     "Lee"
                     "Len"
                     "Lin"
                     "Location"
                     "Ly"
                     "Mah"
                     "Military"
                     "Misdeed"
                     "N"
                     "Nah"
                     "Nature"
                     "Nee"
                     "Nn"
                     "Number"
                     "Occupation"
                     "Oh"
                     "On"
                     "Or"
                     "Orn"
                     "Oth"
                     "Ow"
                     "Ph"
                     "Pr"
                     "R"
                     "Rah"
                     "Ren"
                     "Sah"
                     "Se"
                     "Sh"
                     "Sha"
                     "T"
                     "Ta"
                     "Tal"
                     "Tar"
                     "Th"
                     "Thah"
                     "Thoh"
                     "Ti"
                     "Time"
                     "Tor"
                     "Uh"
                     "Va"
                     "Vah"
                     "Ve"
                     "Vice"
                     "Virtue"
                     "Wah"
                     "Wr"
                     "X"
                     "Y"
                     "Yah"
                     "Yuh"
                     "Z"))
         (name1 (egme--random-list-item name-list))
         (name2 (egme--random-list-item name-list)))

    (egme--print-output (format "Names: %s / %s" name1 name2))))

(defun egme-char-appearance-oracle ()
  
  "Randomly generate a character appearance"
  
  (interactive)
  (let* ((appearance-list (list
                           "Abnormal"
                           "Armed"
                           "Aromatic"
                           "Athletic"
                           "Attractive"
                           "Average"
                           "Bald"
                           "Beautiful"
                           "Bizarre"
                           "Brutish"
                           "Casual"
                           "Classy"
                           "Clean"
                           "Clothing"
                           "Colorful"
                           "Common"
                           "Cool"
                           "Creepy"
                           "Cute"
                           "Dainty"
                           "Delicate"
                           "Desperate"
                           "Different"
                           "Dirty"
                           "Drab"
                           "Elegant"
                           "Equipment"
                           "Exotic"
                           "Expensive"
                           "Extravagant"
                           "Eyewear"
                           "Familiar"
                           "Fancy"
                           "Features"
                           "Feminine"
                           "Festive"
                           "Frail"
                           "Hair"
                           "Hairy"
                           "Headwear"
                           "Heavy"
                           "Hurt"
                           "Innocent"
                           "Insignia"
                           "Intense"
                           "Interesting"
                           "Intimidating"
                           "Jewelry"
                           "Large"
                           "Lavish"
                           "Lean"
                           "Limbs"
                           "Lithe"
                           "Masculine"
                           "Mature"
                           "Messy"
                           "Mighty"
                           "Modern"
                           "Mundane"
                           "Muscular"
                           "Mysterious"
                           "Natural"
                           "Neat"
                           "Normal"
                           "Odd"
                           "Official"
                           "Old"
                           "Petite"
                           "Piercing"
                           "Powerful"
                           "Professional"
                           "Reassuring"
                           "Regal"
                           "Remarkable"
                           "Rough"
                           "Rustic"
                           "Scar"
                           "Scary"
                           "Scented"
                           "Scholarly"
                           "Short"
                           "Simple"
                           "Sinister"
                           "Small"
                           "Smelly"
                           "Stocky"
                           "Strange"
                           "Striking"
                           "Strong"
                           "Stylish"
                           "Tall"
                           "Tattoo"
                           "Tools"
                           "Trendy"
                           "Unusual"
                           "Very"
                           "Weak"
                           "Weapon"
                           "Wounded"
                           "Young"))
         (appearance (egme--random-list-item appearance-list)))

    (egme--print-output (format "Character appearance: %s" appearance))))

(defun egme-char-descriptor-oracle ()
  
  "Randomly generate a character description"
  
  (interactive)
  (let* ((descriptor-list (list
                           "Abnormal"
                           "Active"
                           "Adventurous"
                           "Aggressive"
                           "Agreeable"
                           "Ally"
                           "Ancient"
                           "Angry"
                           "Anxious"
                           "Armed"
                           "Aromatic"
                           "Arrogant"
                           "Attractive"
                           "Awkward"
                           "Beautiful"
                           "Bizarre"
                           "Bleak"
                           "Bold"
                           "Brave"
                           "Busy"
                           "Calm"
                           "Capable"
                           "Careful"
                           "Careless"
                           "Caring"
                           "Cautious"
                           "Cheerful"
                           "Classy"
                           "Clean"
                           "Clumsy"
                           "Colorful"
                           "Combative"
                           "Commanding"
                           "Common"
                           "Competitive"
                           "Confident"
                           "Crazy"
                           "Curious"
                           "Dangerous"
                           "Different"
                           "Difficult"
                           "Dirty"
                           "Disagreeable"
                           "Disciplined"
                           "Educated"
                           "Elegant"
                           "Erratic"
                           "Exotic"
                           "Fancy"
                           "Fast"
                           "Foul"
                           "Frightened"
                           "Gentle"
                           "Harmful"
                           "Helpful"
                           "Heroic"
                           "Humorous"
                           "Hurt"
                           "Ignorant"
                           "Impulsive"
                           "Inept"
                           "Informative"
                           "Intelligent"
                           "Interesting"
                           "Intimidating"
                           "Intrusive"
                           "Large"
                           "Loud"
                           "Meek"
                           "Naive"
                           "Old"
                           "Passive"
                           "Polite"
                           "Poor"
                           "Powerful"
                           "Powerless"
                           "Primitive"
                           "Principled"
                           "Quiet"
                           "Respectful"
                           "Rough"
                           "Rude"
                           "Simple"
                           "Skilled"
                           "Slow"
                           "Small"
                           "Sneaky"
                           "Sophisticated"
                           "Strange"
                           "Strong"
                           "Supportive"
                           "Surprising"
                           "Sweet"
                           "Trained"
                           "Uniformed"
                           "Unusual"
                           "Weak"
                           "Wealthy"
                           "Wild"
                           "Young"))
         (descriptor (egme--random-list-item descriptor-list)))

    (egme--print-output (format "Character descriptors: %s" descriptor))))

(defun egme-char-personality-oracle ()

  "Randomly generate a character personality"

  (interactive)
  (let* ((personality-list (list
                            "Active"
                            "Adventurous"
                            "Aggressive"
                            "Agreeable"
                            "Ambitious"
                            "Amusing"
                            "Angry"
                            "Annoying"
                            "Anxious"
                            "Arrogant"
                            "Average"
                            "Awkward"
                            "Bad"
                            "Bitter"
                            "Bold"
                            "Brave"
                            "Calm"
                            "Careful"
                            "Careless"
                            "Classy"
                            "Cold"
                            "Collector"
                            "Committed"
                            "Competitive"
                            "Confident"
                            "Control"
                            "Crazy"
                            "Creative"
                            "Crude"
                            "Curious"
                            "Deceptive"
                            "Determined"
                            "Devoted"
                            "Disagreeable"
                            "Dull"
                            "Emotion"
                            "Empathetic"
                            "Fair"
                            "Fastidious"
                            "Follower"
                            "Foolish"
                            "Friendly"
                            "Good"
                            "Gourmet"
                            "Greed"
                            "Haunted"
                            "Helpful"
                            "Honest"
                            "Honor"
                            "Humble"
                            "Humorous"
                            "Inconsistent"
                            "Independent"
                            "Interesting"
                            "Intolerant"
                            "Irresponsible"
                            "Knowledgeable"
                            "Larcenous"
                            "Leader"
                            "Likable"
                            "Loyal"
                            "Manipulative"
                            "Mercurial"
                            "Naive"
                            "Nervous"
                            "Oblivious"
                            "Obstinate"
                            "Optimistic"
                            "Perceptive"
                            "Perfectionist"
                            "Practical"
                            "Prepared"
                            "Principled"
                            "Protect"
                            "Quiet"
                            "Quirky"
                            "Rash"
                            "Rational"
                            "Respectful"
                            "Responsible"
                            "Restless"
                            "Risk"
                            "Rude"
                            "Savvy"
                            "Searching"
                            "Selfish"
                            "Selfless"
                            "Shallow"
                            "Social"
                            "Strange"
                            "Strong"
                            "Studious"
                            "Superstitious"
                            "Tolerant"
                            "Vindictive"
                            "Vocal"
                            "Wary"
                            "Weak"
                            "Wild"
                            "Wise"))
         (personality (egme--random-list-item personality-list)))

    (egme--print-output (format "Character personality: %s" personality))))

(defun egme-char-conversations-oracle ()

  "Randomly generate a character's conversational attitude."

  (interactive)
  (let* ((conversations-list (list
                              "Abuse"
                              "Advice"
                              "Aggressive"
                              "Agree"
                              "Amusing"
                              "Angry"
                              "Anxious"
                              "Assist"
                              "Awkward"
                              "Betray"
                              "Bizarre"
                              "Bleak"
                              "Bold"
                              "Business"
                              "Calm"
                              "Careful"
                              "Careless"
                              "Cautious"
                              "Cheerful"
                              "Classy"
                              "Cold"
                              "Colorful"
                              "Combative"
                              "Crazy"
                              "Creepy"
                              "Curious"
                              "Defiant"
                              "Delightful"
                              "Disagreeable"
                              "Dispute"
                              "Efficient"
                              "Energetic"
                              "Enthusiastic"
                              "Excited"
                              "Fearful"
                              "Fierce"
                              "Foolish"
                              "Frantic"
                              "Frightening"
                              "Generous"
                              "Gentle"
                              "Glad"
                              "Grateful"
                              "Haggle"
                              "Happy"
                              "Harsh"
                              "Hasty"
                              "Helpful"
                              "Helpless"
                              "Hopeless"
                              "Ideas"
                              "Inform"
                              "Innocent"
                              "Inquire"
                              "Intense"
                              "Interesting"
                              "Intolerance"
                              "Irritating"
                              "Joyful"
                              "Judgmental"
                              "Juvenile"
                              "Kind"
                              "Leadership"
                              "Lie"
                              "Loud"
                              "Loving"
                              "Loyal"
                              "Macabre"
                              "Mature"
                              "Meaningful"
                              "Miserable"
                              "Mistrust"
                              "Mocking"
                              "Mundane"
                              "Mysterious"
                              "News"
                              "Nice"
                              "Normal"
                              "Odd"
                              "Offensive"
                              "Official"
                              "Oppose"
                              "Peace"
                              "Plans"
                              "Playful"
                              "Polite"
                              "Positive"
                              "Praise"
                              "Quarrelsome"
                              "Quiet"
                              "Reassuring"
                              "Refuse"
                              "Rude"
                              "Rumor"
                              "Simple"
                              "Threatening"
                              "Truce"
                              "Trust"
                              "Warm"
                              "Wild"))
         (conversations (egme--random-list-item conversations-list)))

    (egme--print-output (format "Character conversations: %s" conversations))))

(defun egme-add-npc (&optional npc-name)
  "This function adds an NPC to the current file.

NPCS are stored at the end of the file, under an :NPCS: drawer. It will search backwards from the end of the file for the drawer, and create it if not found. npc-name is then inserted on at the beginning of the drawer."

  (interactive)

  ;; Ask for NPC name if nothing is passed to the function
  (if npc-name
      t
    (setq npc-name (read-string "New NPC name? ")))

  ;; save-excursion so cursor returns to users current position
  (save-excursion
    (progn
      (end-of-buffer)
      
      ;; Search backwards for ":NPCS:" 
      (if (search-backward ":NPCS:" nil t)

	  ;; The drawer has been found, check if npc-name already exists - add if missing, throw an error if it already exists
	  (if (member npc-name (egme--parse-npc-list))
	      (user-error "NPC is already in the list")
	    (progn
	      (egme--open-org-drawer)
	      (end-of-line)
	      (newline)
	      (insert npc-name)))

	;; The :NPCS: drawer doesn't exist, create it and add the new npc-name
	(insert (concat "\n:NPCS:\n" npc-name "\n:END:\n")))

      ;; Fold the Drawer closed
      (search-backward ":NPCS:" nil t)
      (egme--close-org-drawer)))

  ;; Refresh dispay buffer if open
  (egme--update-display-buffer)
  
  ;; Return the added npc-name
  npc-name)

(defun egme--parse-npc-list ()
    "This function gets locates the NPC list in the given file, and store all the names in the list egme-npc-list

If the :NPCS: drawer cannot be found, then an error message will be created, and the function returns nil. Otherwise, the generated list will be returned (in addtion to being added to egme-npc-list variable)."

    (setq egme-npc-list nil)
        
  (save-excursion
    (progn
      (end-of-buffer)

      ;; Find NPC drawer
      (if (search-backward ":NPCS:" nil t)

	  ;; Drawer found, turn it into a list
	  (progn
	    ;; Open drawer before parsing
	    (egme--open-org-drawer)
	    (next-line)

	    ;; Loop until end of drawer found
	    (while (not (string-match "^:END:" (thing-at-point 'line t)))
	      (progn
		;; Add current element, minus final character (trailing newline), then move to next
		(push (substring (thing-at-point 'line t) 0 -1) egme-npc-list)
		(next-line)))

	    ;; Close the drawer again
	    (search-backward ":NPCS:" nil t)
	    (egme--close-org-drawer))

	;; No NPC drawer found
	(message "No NPC list in current file"))))

  ; Return list contents (or nil if nothing is found)
  egme-npc-list)

(defun egme-delete-npc ()
  "This function deletes an NPC from the active list.

The NPC list is parsed, and all are offered as options with ido-completing-read. This is then found within the NPC list drawer, and the chosen option is deleted. This function then re-parses and returns the updated list."

  (interactive)

  ;; Check NPC list has been created
  (if (egme--parse-npc-list)
      
      ;; Parse latest NPC list, and get user input for which to delete
      (setq deleting-npc (ido-completing-read "NPC to delete? " (egme--parse-npc-list)))
    
    ;; Throw an error if nothing found
    (user-error "No NPCs in current file"))

  (save-excursion
    (progn

      ;; Go to end of buffer, then look backwards for the NPC list and open it
      (end-of-buffer)
      (search-backward ":NPCS:" nil t)
      (egme--open-org-drawer)

      ;; Search forwards for the selected deletion
      (search-forward deleting-npc nil t)
      (beginning-of-line)

      ;; Delete line and remove the newline to avoid a blank entry
      (kill-line)
      (kill-line)

      ;; Close the NPC drawer
      (search-backward ":NPCS:" nil t)
      (egme--close-org-drawer)))

  ;; Refresh dispay buffer if open
  (egme--update-display-buffer)
  
  ;; Return updated list
  (egme--parse-npc-list))

(defun egme-add-thread (&optional new-thread)
  "This function adds a Thread to the current file.

Threads are stored at the end of the file, under an :THREADS: drawer. It will search backwards from the end of the file for the drawer, and create it if not found. new-thread is then inserted on at the beginning of the drawer."

  (interactive)

  ;; Ask for Thread if nothing is passed to the function
  (if new-thread
      t
    (setq new-thread (read-string "New Thread description? ")))

  ;; save-excursion so cursor returns to users current position
  (save-excursion
    (progn
      (end-of-buffer)
      
      ;; Search backwards for ":THREADS:" 
      (if (search-backward ":THREADS:" nil t)

	  ;; The drawer has been found, check if new-thread already exists - add if missing, throw an error if it already exists
	  (if (member new-thread (egme--parse-thread-list))
	      (user-error "Thread is already in the list")
	    (progn
	      (egme--open-org-drawer)
	      (end-of-line)
	      (newline)
	      (insert new-thread)))

	;; The :THREADS: drawer doesn't exist, create it and add the new-thread
	(insert (concat "\n:THREADS:\n" new-thread "\n:END:\n")))

      ;; Fold the Drawer closed
      (search-backward ":THREADS:" nil t)
      (egme--close-org-drawer)))

  ;; Refresh dispay buffer if open
  (egme--update-display-buffer)
  
  ;; Return the added new-thread
  new-thread)

(defun egme--parse-thread-list ()
    "This function gets locates the Thread list in the given file, and store all the items in the list egme-thread-list

If the :THREADS: drawer cannot be found, then an error message will be created, and the function returns nil. Otherwise, the generated list will be returned (in addtion to being added to egme-thread-list variable)."

    (setq egme-thread-list nil)
        
  (save-excursion
    (progn
      (end-of-buffer)

      ;; Find Thread drawer
      (if (search-backward ":THREADS:" nil t)

	  ;; Drawer found, turn it into a list
	  (progn
	    ;; Open drawer before parsing
	    (egme--open-org-drawer)
	    (next-line)

	    ;; Loop until end of drawer found
	    (while (not (string-match "^:END:" (thing-at-point 'line t)))
	      (progn
		;; Add current element, minus final character (trailing newline), then move to next
		(push (substring (thing-at-point 'line t) 0 -1) egme-thread-list)
		(next-line)))

	    ;; Close the drawer again
	    (search-backward ":THREADS:" nil t)
	    (egme--close-org-drawer))

	;; No THREADS drawer found
	(message "No Thread list in current file"))))

  ; Return list contents (or nil if nothing is found)
  egme-thread-list)

(defun egme-delete-thread ()
  "This function deletes a Thread from the active list.

The Thread list is parsed, and all are offered as options with ido-completing-read. This is then found within the Thread list drawer, and the chosen option is deleted. This function then re-parses and returns the updated list."

  (interactive)

  ;; Check Thread list has been created
  (if (egme--parse-thread-list)
      
      ;; Parse latest Thread list, and get user input for which to delete
      (setq deleting-thread (ido-completing-read "Thread to delete? " (egme--parse-thread-list)))
    
    ;; Throw an error if nothing found
    (user-error "No Threads in current file"))

  (save-excursion
    (progn

      ;; Go to end of buffer, then look backwards for the Thread list and open it
      (end-of-buffer)
      (search-backward ":THREADS:" nil t)
      (egme--open-org-drawer)

      ;; Search forwards for the selected deletion
      (search-forward deleting-thread nil t)
      (beginning-of-line)

      ;; Delete line and remove the newline to avoid a blank entry
      (kill-line)
      (kill-line)

      ;; Close the Thread drawer
      (search-backward ":THREADS:" nil t)
      (egme--close-org-drawer)))

  ;; Refresh dispay buffer if open
  (egme--update-display-buffer)
  
  ;; Return updated list
  (egme--parse-thread-list))

  (defun egme-dashboard ()
    "This function will create a temporary buffer to display current details of the game state.

  At present this is the NPC list & Thread list (formatted 1 item per line), and the results of the last dice roll.

  This function always returns nil."

    (interactive)

    ;; Update all lists from curent file
    (egme--parse-npc-list)
    (egme--parse-thread-list)

    ;; Remember old window split thresholds, and change current to 1 too force a horizontal split
    (setq egme-old-threshold split-width-threshold)
    (setq split-width-threshold 1)

    ;; Save location in current buffer
    (save-excursion

      ;; Create temporary read-only buffer and move to it for output
      (with-output-to-temp-buffer "GameMaster"
        (set-buffer "GameMaster")

        ;; Print header
        (org-mode)
        (insert "*eGME- Emacs GameMaster Emulator*\n---\n\n\n")

        ;; Check if NPC list is empty
        (if (not egme-npc-list)

            ;; Output when no list found
            (insert "No NPCs at present")

          ;; Output when NPCs found
          (progn
            (insert "NPCs\n---\n")

            ;; Loop through NPC list
            (while egme-npc-list

              ;; Pop the list, using each item as output followed by newline
              (insert (pop egme-npc-list))
              (newline))))

        (newline)
        (newline)
	

        ;; Check if Thread list is empty
        (if (not egme-thread-list)

            ;; Output when no list found
            (insert "No Threads at present\n")

          ;; Output when Threads found
          (progn
            (insert "Threads\n---\n")

            ;; Loop through Thread list
            (while egme-thread-list

              ;; Pop the list, using each item as output followed by newline
              (insert (pop egme-thread-list))
              (newline))))

	    
	;; Insert last dice roll info
	(insert "\n\nLast Dice Roll\n---\n")
	(insert (concat (format "  Roll: %s" egme-current-dice) (format "\nResult: %s" egme-roll-result))))

      ;; Switch to the new window, temporarily alow horizontal changes, and shrink it to fit the contents
      (other-window 1)
      (set-variable 'fit-window-to-buffer-horizontally 1)
      (fit-window-to-buffer)
      ;; Make it a bit bigger because it was shrinking too much...
      (enlarge-window-horizontally 7)
      (other-window 1))

    ;; Return to original split settings
    (setq split-width-threshold egme-old-threshold)

    nil)

(defun egme--update-display-buffer ()
  "Simple function to reopen the game-state display if it is visible.

This is to be called at the end of anything that changes displayed information."

  (if (get-buffer-window "GameMaster")
      (egme-dashboard)))

(defun egme-toggle-dash ()
  "This function will toggle egme dashboard visibilty.

If it is visible it will kill the window, if not it will load it and update it."

  (interactive)

  (if (get-buffer-window "GameMaster")
      (progn
	(delete-window (get-buffer-window "GameMaster"))
	(kill-buffer "GameMaster"))
    (egme-dashboard)))

(define-prefix-command 'egme-map)
(define-key mode-specific-map (kbd "C-g") 'egme-map)
(define-key egme-map (kbd "r") 'egme-roll-dice)
(define-key egme-map (kbd "q") 'egme-y-n-oracle)
(define-key egme-map (kbd "n") 'egme-add-npc)
(define-key egme-map (kbd "N") 'egme-delete-npc)
(define-key egme-map (kbd "t") 'egme-add-thread)
(define-key egme-map (kbd "T") 'egme-delete-thread)
(define-key egme-map (kbd "d") 'egme-toggle-dash)
