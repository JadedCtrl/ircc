;;
;; Copyright 2022, Jaidyn Levesque <jadedctrl@posteo.at>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;


(import scheme
		(chicken io) (chicken tcp)
		srfi-1 srfi-69 srfi-130
		openssl)


;; ——————————————————————————————————————————————————————————————————————————————
;; Main
;; ——————————————————————————————————————————————————————————————————————————————

;; Connect to the given IRC server, returning an IRC connection object.
(define (irc:connect host port username nick #!optional (password #f) (realname #f))
  (let ([conn (make-hash-table)])
	(define-values (out in)
	  (ssl-connect* hostname: host port: port))
	(hash-table-set! conn 'in in)
	(hash-table-set! conn 'out out)
	(hash-table-set! conn 'username username)
	(hash-table-set! conn 'nick nick)
	(hash-table-set! conn 'realname realname)
	(hash-table-set! conn 'channels (make-hash-table))

	(if password
		(irc:write-cmd conn "PASS" password))
	(irc:write-cmd conn "USER" username "*" "0"
				   (if realname realname "Jane Row"))
	(irc:write-cmd conn "NICK" nick)
	conn))


;; Basic loop for using an IRC connection, using two hook functions:
;;    (on-command connection command params sender)
;;    (on-reply connection reply-code params sender)
(define (irc:loop connection on-command on-reply)
  (let* ([output (irc:read-alist connection)]
		 [command (alist-ref 'command output)]
		 [reply (alist-ref 'reply output)]
		 [params (alist-ref 'params output)]
		 [sender (alist-ref 'sender output)])
	(if (and on-command (car command))
		(apply on-command (append (list connection) command (list params) sender)))
	(if (and on-reply (car reply))
		(apply on-reply (append (list connection) reply (list params) sender)))
	(irc:loop connection on-command on-reply)))


;; ——————————————————————————————————————————————————————————————————————————————
;; I/O
;; ——————————————————————————————————————————————————————————————————————————————

;; Read-in the next reply or command from the server, into a parsable alist with
;; four keys:
(define (irc:read-alist conn)
  (irc:process-alist-internally
   conn
   (irc:line->alist (irc:read-line conn))))


;; Read a single line from the IRC server
(define (irc:read-line conn)
  (read-line (hash-table-ref conn 'out)))


;; Send a specific command to the server.
(define (irc:write-cmd conn command . parameters)
  (irc:write-line (apply irc:cmd->string (append `(,command) parameters))
				  conn))


;; Write a line to the IRC server connection.
(define (irc:write-line text connection)
  (write-line text (hash-table-ref connection 'in)))


;; ——————————————————————————————————————————————————————————————————————————————
;; Processing/saving metadata
;; ——————————————————————————————————————————————————————————————————————————————

;; The user should have more-or-less total control over how to respond to
;; received messages, but ircc has to sneakily process some responses itself,
;; to ensure basic functionality (i.e., pings, chanlist, userlist, etc.)
(define (irc:process-alist-internally conn alist)
  (let ([command (alist-car-ref 'command alist)]
		[reply (alist-car-ref 'reply alist)]
		[sender (alist-car-ref 'sender alist)]
		[params (alist-ref 'params alist)])
	(if command
		(irc:process-command-internally conn command params sender)
		(irc:process-reply-internally conn reply params sender)))
  alist)


;; Handle some replies necssary for basic functionality
(define (irc:process-reply-internally conn reply params #!optional sender)
  (cond [(eq? reply RPL_WELCOME)
		 (hash-table-set! conn 'registered #t)
		 (hash-table-set! conn 'nick (car params))]
		[(eq? reply RPL_TOPIC)
		 (let ([channel (second params)]
			   [topic (last params)])
		   (irc:channel-set! conn channel 'topic topic))]
		[(eq? reply RPL_TOPICWHOTIME)
		 (let ([channel (second params)]
			   [setter-nick (third params)]
			   [time (time-unix->time-utc (string->number (last params)))])
		   (irc:channel-set! conn channel 'topic-set
							 (time->date time)))]
		[(eq? reply RPL_NAMREPLY)
		 (let ([channel (third params)]
			   [chan-symbol (second params)]
			   [users (cdddr params)])
		   (irc:channel-set! conn channel 'symbol chan-symbol)
		   (apply
			(lambda (user)
			  (irc:channel-user-add! conn channel user))
			users))]))


;; Handle some commands necessary for basic functionality
(define (irc:process-command-internally conn command params #!optional sender)
  (cond [(string=? command "PING")
		 (irc:write-cmd conn "PONG" (last params))]
		[(string=? command "JOIN")
 		 (let ([room-name (car params)]
			   [new-user (car (string-split sender "!"))])
		   (if (irc:user-is-self? conn new-user)
			   (irc:channel-add! conn room-name))
		   (irc:channel-user-add! conn room-name new-user))]))


;; ——————————————————————————————————————————————————————————————————————————————
;; Metadata accessors
;; ——————————————————————————————————————————————————————————————————————————————

;; Return whether or not the given string (username/nick/whatever) is equivalent
;; to current user.
(define (irc:user-is-self? conn user-string)
  (or (string=? (hash-table-ref conn 'username) user-string)
	  (string=? (hash-table-ref conn 'nick)
				(car (string-split user-string "!")))
	  (string=? (hash-table-ref conn 'client) user-string)))


;; Add a channel of name `chan` to the internal list of channels
(define (irc:channel-add! conn chan)
  (hash-table-set! (hash-table-ref conn 'channels) chan (make-hash-table))
  (hash-table-set! (irc:channel-table conn chan) 'users '()))


;; Remove a channel of name `chan` from the internal list of channels
(define (irc:channel-remove! conn chan)
  (hash-table-remove! (hash-table-ref conn 'channels) chan))


;; Return a list of saved channels by name
(define (irc:channels conn)
  (hash-table-keys (hash-table-ref conn 'channels)))


;; Return a saved channel's table
(define (irc:channel-table conn chan)
  (hash-table-ref (hash-table-ref conn 'channels) chan))


;; Get a stored value associated with a channel, by key
(define (irc:channel-get conn chan key)
  (hash-table-ref (irc:channel-table conn chan) key))


;; Associate a value with a given channel, by key
(define (irc:channel-set! conn chan key value)
  (hash-table-set! (irc:channel-table conn chan)
				   key value))


;; Returns a list of users that are stored as members of the given channel
(define (irc:channel-users conn chan)
  (irc:channel-get conn chan 'users))


;; Add a user to a channel's list of users, by nick
(define (irc:channel-user-add! conn chan nick)
  (irc:channel-set!
   conn chan 'users
   (append (irc:channel-get conn chan 'users)
		   (list nick))))


;; Remove a user from a channel's list of users, by nick
(define (irc:channel-user-del! conn chan nick)
  (irc:channel-set!
   conn chan 'users
   (filter (lambda (a-nick)
			 (not (string=? nick a-nick)))
		   (irc:channel-users conn name))))


;; —————————————————————————————————————————————————————————————————————————————
;; Parsing lines/commands
;; —————————————————————————————————————————————————————————————————————————————

;; Construct a string to write to IRC for the given command and parameters.
(define (irc:cmd->string command . parameters)
  (let ([parameters
		 (append (reverse (cdr (reverse parameters)))
				 `(,(string-append ":" (last parameters))))])
	(string-append
	 command
	 " "
	 (reduce-right
	  (lambda (a b)
		(string-append a " " b))
	  #f
	  parameters))))


;; Convert a string to a `msg` alist, with keys 'command', 'reply', 'params',
;; and 'sender'.
(define (irc:line->alist str)
  (let* ([space-split (string-split str " ")]
		 [tags (irc:line-tags str space-split)]
		 [sender (irc:line-sender str space-split)]
		 [verb (irc:line-verb str space-split)]
		 [command (car verb)]
		 [reply (string->number (car verb))]
		 [params (irc:line-verb-params verb)])
	`((command ,(if (not reply) command #f))
	  (reply ,reply)
	  ,(append '(params) params)
	  (sender ,sender)
	  ,(append '(tags) tags))))


;; Parses out all tags from the given line of IRC output
(define (irc:line-tags str space-split)
  (if (not (string=? (string-take str 1) "@"))
	  #f
	  (let*
		  ([first-column (car space-split)]
		   [tag-strs (string-split (string-drop first-column 1) ";")]
		   [tag-pairs (map
					   (lambda (tag-str)
						 (string-split tag-str "="))
					   tag-strs)]
		   [no-empty-pairs (map
							(lambda (tag-pair)
							  (if (eq? (length tag-pair) 1)
								  (append tag-pair '(""))
								  tag-pair))
							tag-pairs)]
		   [escaped-pairs
			(map
			 (lambda (tag-pair)
			   (list (car tag-pair)
					 (string-translate* (cadr tag-pair)
										'(("\\s" . " ")
										  ("\\\\" . "\\")
										  ("\\r" . "\r")
										  ("\\n" . "\n")))))
			 no-empty-pairs)])
		escaped-pairs)))


;; Parse the sender of an IRC output line, if there is any
(define (irc:line-sender str space-split)
  (let ([first-char (string-take str 1)])
	(cond
	 [(and (string=? first-char "@")
		   (string=? (string-take (cadr space-split) 1) ":"))
	  (string-drop (cadr space-split) 1)]
	 [(string=? first-char ":")
	  (string-drop (car space-split) 1)]
	 [#t
	  #f])))


;; Parse out the verb (command or reply) with subsequent words into a list
(define (irc:line-verb str space-split)
  (let ([first-char (string-take str 1)])
	(cond
	 [(and (string=? first-char "@")
		   (string=? (string-take (cadr space-split) 1) ":"))
	  (cddr space-split)]
	 [(or (string=? first-char "@")
		  (string=? first-char ":"))
	  (cdr space-split)]
	 [#t
	  space-split])))


;; Returns a list of parameters from the parsed-out verb section of a line
(define (irc:line-verb-params verb)
  (let* ([params (cdr verb)]
		[other-params '()]
		[last-param '()])
	(map (lambda (param)
		   (cond
			[(and (string=? (string-take param 1) ":")
				  (null? last-param))
			 (set! last-param
			   (append last-param `(,(string-drop param 1))))]
			[(not (null? last-param))
			 (set! last-param (append last-param `(,param)))]
			[#t
			 (set! other-params (append other-params `(,param)))]))
		 params)
	(append
	 other-params
	 `(,(reduce-right
		 (lambda (a b)
		   (string-append a " " b))
		 #f
		 last-param)))))


;; —————————————————————————————————————————————————————————————————————————————
;; Misc. helpers
;; —————————————————————————————————————————————————————————————————————————————

;; Just car's the value of alist-ref (if it exists)
(define (alist-car-ref key alist)
  (let ([value (alist-ref key alist)])
	(if value
		(car value)
		#f)))


;; By Göran Weinholt, from the Scheme Cookbook
;; https://cookbook.scheme.org/format-unix-timestamp/
(define (time-unix->time-utc seconds)
  (add-duration (date->time-utc (make-date 0 0 0 0 1 1 1970 0))
                (make-time time-duration 0 seconds)))


;; By Göran Weinholt, from the Scheme Cookbook
;; https://cookbook.scheme.org/format-unix-timestamp/
(define (time-unix->string seconds . maybe-format)
  (apply date->string (time-utc->date (time-unix->time-utc seconds))
         maybe-format))


;; —————————————————————————————————————————————————————————————————————————————
;; IRC constants
;; —————————————————————————————————————————————————————————————————————————————

(define RPL_WELCOME 1)
(define RPL_WHOISUSER 311)
(define RPL_ENDOFWHO 315)
(define RPL_ENDOFWHOIS 318)
(define RPL_LIST 322)
(define RPL_LISTEND 323)
(define RPL_TOPIC 332)
(define RPL_WHOREPLY 352)
(define RPL_NAMREPLY 353)
(define RPL_MOTD 372)
(define RPL_MOTDSTART 375)
(define RPL_ENDOFMOTD 376)
(define ERR_NONICKNAMEGIVEN 431)
(define ERR_ERRONEUSNICKNAME 432)
(define ERR_NICKNAMEINUSE 433)
