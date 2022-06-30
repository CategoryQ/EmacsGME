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

(defcustom egme-print-line-start "~~|Games Master|~~~~~~~~~"
  "String printed before games master ouput in non-org buffers."
  :type 'string
  :group 'egme)

(defcustom egme-print-line-end "~~~~~~~~~~~~~~~~~~~~~~~~~"
  "String printed after games master output in non-org buffers."
  :type 'string
  :group 'egme)

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

(setq egme-dice-history (list))

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

(setq egme-random-event-list (list
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

(defun egme-random-list-item (list-to-pick-from)
  "This function takes a list as an argument, and returns a random element from within that list.

Will return nil if provided list is nil."

  (cond
    (list-to-pick-from (nth (random (length list-to-pick-from)) list-to-pick-from))
    (t nil)))

  (defun egme-get-dice ()
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

(defun egme-calculate-dice (&optional dice-roll)
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

(defun egme-print-output (print-string)
  "This function takes a string in as an argument, and prints it's output into the current buffer, between lines highlighting it as games-master output.

For normal text files, the visual braces are stored as the following strings:-

  egme-print-line-start
  egme-print-line-end

If the current buffer is an org-mode document, the output is placed inside a quote block so it can retain the bonuses of export fomatting."

  ;; Move point to "safe" position
  (end-of-line)

  ;; Add additional newline if current line contains is non-blank
  (when (string-match "[:ascii:]" (thing-at-point 'line t))
      (newline))

  (newline)

  ;;; Output the start line
  ;; Check if current buffer is an org-mode file, and output accordingly
  (if (equal (with-current-buffer (current-buffer) major-mode) 'org-mode)
      ;; If an org-file, output into a quote block
      (insert "#+BEGIN_QUOTE GamesMaster")
    ;; Else output the opening brace
    (insert egme-print-line-start))

  (newline)

  ;; Output text generated by egme functions
  (insert print-string)

  (newline)

  ;;;; Output the end line
  ;; Check if current buffer is an org-mode file
  (if (equal (with-current-buffer (current-buffer) major-mode) 'org-mode)
      ;; If an org-file, close the quote block
      (insert "#+END_QUOTE GamesMaster")
    ;; Else output the closing brace brace
    (insert egme-print-line-end))
  
  (newline 2)

  t)

  (defun egme-random-event ()
    "A function for genereating unexpected events.

When an oracle question is asked, this function is called. It keeps a counter in the variable egme-random-counter, which is incremented easch time this is called. Then a single 1d20 is rolled - if the result is lower than the current egme-random-counter value, then a random event is generated. A focus, action and subject are randomly selected from the lists (egme-random-event-list, egme-action-list, and egme-subject-list respectively). If a random event was generated, the counter is reset to 0.

This function then returns the random event text, for the calling function to pass on to for user output."

    ;; Increment random counter
    (setq egme-random-counter (1+ egme-random-counter))

    ;; Clear random event output text
    (setq egme-random-event-output nil)
  
    (cond
      ;; Compare the random counter to a d20 roll
      ((< (egme-calculate-dice "1d20") egme-random-counter)

       ;; Announce the event
       (setq egme-random-event-output "\n------------\nRandom Event!")
      
        ;; Add a type of random event
        (setq egme-random-event-output (concat egme-random-event-output (format "\n      Focus:  %s" (egme-random-list-item egme-random-event-list))))

	;; Add event details
	(setq egme-random-event-output (concat egme-random-event-output (format "\n     Detail:  %s" (egme-random-list-item egme-action-list))(format " / %s" (egme-random-list-item egme-subject-list))))

	;; Reset the random counter
	(setq egme-random-counter 0)

	;; Return text output
	egme-random-event-output)))

(defun egme-open-org-drawer ()
  "This function will open an org-mode drawer on the current line, if it is currently closed.

Open state is determined by checking if current line is a drawer, and if the text at the end of the line is visible. If it is invisible, open the drawer with org-cycle."

  (if (and (org-at-drawer-p) (invisible-p (point-at-eol)))
      (org-cycle)
    (user-error "No closed drawer to open")))

(defun egme-close-org-drawer ()
  "This function will close an org-mode drawer on the current line, if it is currently open.

Open state is determined by checking if current line is a drawer, and if the text at the end of the line is visible. If it is not invisible, close the drawer with org-cycle."

  (if (and (org-at-drawer-p) (not (invisible-p (point-at-eol))))
      (org-cycle)
    (user-error "No open drawer to close")))

(defun egme-roll-dice ()
  "This function is for a user to generate the results from a dice roll, and output them into the current buffer.

egme-get-dice is called to get user input, egme-calculate dice is used to generate the result, and egme-print-output is used to place this into the current buffer, creating new lines below the point.

This function is interactively callable via M-x, and a prime input option for key-binding."
  ; Let user call via M-x
  (interactive)

  ; Get dice size from user
  (egme-get-dice)

  ; Check dice input was correct
  (if egme-current-dice
    ; If valid then calculate result
    (egme-calculate-dice)
    ; Else drop an error message and exit
    (user-error "Could not parse dice roll"))

  ; Print results
  (egme-print-output (concat (format "Rolled:  %s" egme-current-dice) (format "\nResult:  %s" egme-roll-result)))
  egme-roll-result)

  (defun egme-y-n-oracle ()
    "The basic oracle function. This will provide Yes/No answers to questions posed to the games master, and outputs the results in the current buffer in the standard games master format.

The user will be asked to input a question - if the end of the current line is parsed as a question, then that will be set as the initial user input. If a quesiton is provided, it will be printed along with the results.
Next, the user will be asked for the likelihood of this result. These options are stored in the list egme-probability-list, and selected via ido-completing-read. Each option is a modifier between -4 and +4, along with a basic description of the probability. This basic description will be printed along with the results.
The answer is generated by rolling 1D10 and applying the chosen modifier, any result of a 6+ will be a 'Yes', anything else a 'No'. A D6 is also rolled, to see if it is an extreme answer - on a 1 it is a minor result (', but...'), and on a 2 it is a major result (', and...').

The function egme-random-event is also called to see if anything unexpected occurs - any change will be added to the variable egme-oracle-output before it gets passed on for user output."
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
    (setq egme-oracle-answer-roll (+ (egme-calculate-dice "1d10") (string-to-number egme-current-probability-modifier)))
    (setq egme-oracle-answer-modifier (egme-calculate-dice "1d6"))

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
    (setq egme-oracle-output (concat egme-oracle-output (egme-random-event)))

    ; Send output string to display to user 
    (egme-print-output egme-oracle-output))

(defun egme-add-npc (&optional npc-name)
  "This function adds an NPC to the current file.

NPCS are stored at the end of the file, under an :NPCS: drawer. It will search backwards from the end of the file for the drawer, and create it if not found. npc-name is then inserted on at the beginning of the drawer."

  (interactive)

  ;; Ask for NPC name if nothing is passed to the function
  (if npc-name
      t
    (setq npc-name (read-string "New NPC name?")))

  ;; save-excursion so cursor returns to users current position
  (save-excursion
    (progn
      (end-of-buffer)
      
      ;; Search backwards for ":NPCS:" 
      (if (search-backward ":NPCS:" nil t)

	  ;; The drawer has been found, check if npc-name already exists
	  (progn
	    (egme-open-org-drawer)
	    (end-of-line)
	    (newline)
	    (insert npc-name))

	;; The :NPCS: drawer doesn't exist, create it and add the new npc-name
	(insert (concat "\n:NPCS:\n" npc-name "\n:END:\n")))

      ;; Fold the Drawer closed
      (search-backward ":NPCS:" nil t)
      (egme-close-org-drawer)))
  
  ;; Return the added npc-name
  npc-name)

(defun egme-parse-npc-list ()
    "This function gets locates the NPC list in the given file, and store all the names in the list egme-npc-list

If the :NPC: drawer cannot be found, then an error message will be created, and the function returns nil. Otherwise, the generated list will be returned (in addtion to being added to egme-npc-list variable)."

  ; Clear any existing data from the stored list variable.
  (setq egme-npc-list (list))

  (save-excursion
    (progn
      (end-of-buffer)

      ;; Find NPC drawer
      (if (search-backward ":NPCS:" nil t)

	  ;; Drawer found, turn it into a list
	  (progn
	    ;; Open drawer before parsing
	    (egme-open-org-drawer)
	    (next-line)

	    ;; Loop until end of drawer found
	    (while (not (string-match "^:END:" (thing-at-point 'line t)))
	      (progn
		;; Add current element, minus final character (trailing newline), then move to next
		(push (substring (thing-at-point 'line t) 0 -1) egme-npc-list)
		(next-line)))

	    ;; Close the drawer again
	    (search-backward ":NPCS" nil t)
	    (egme-close-org-drawer))

	;; No NPC drawer found
	(user-error "No NPC list in current file"))))

  ; Return list contents (or nil if nothing is found)
  egme-npc-list)

(define-prefix-command 'egme-map)
(define-key mode-specific-map (kbd "C-g") 'egme-map)
(define-key egme-map (kbd "r") 'egme-roll-dice)
(define-key egme-map (kbd "q") 'egme-y-n-oracle)
(define-key egme-map (kbd "n") 'egme-add-npc)
