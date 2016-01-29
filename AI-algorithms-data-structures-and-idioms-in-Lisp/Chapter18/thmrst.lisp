;;;; AI algorithms, data structures, and idioms in Prolog, Lisp, and Java
;;;; thmrst.lisp - Chapter18 - Semantic Networks, Inheritance, and CLOS
;;;; A thermostat Simulation.
;;;; Edited by: George Lukas < last update: Jan/20/2016 - 07:57PM >

;;; A general thermostat class.
(defclass thermostat ()
  ((setting :initform 65
	    :accessor therm-setting)))

;;; a thermostat designed to control heaters
(defclass heater-thermostat (thermostat)
  ((heater :allocation :class
	   :initarg heater-obj)))

;;; the heater class
(defclass heater ()
  ((state :initform 'off
	  :accessor heater-state)
   (location :initarg loc)
   (rooms-heated)))

;;; a room class 
(defclass roomq ()
  ((temperature :initform 65
		:accessor room-temp)
   (thermostat :initarg therm
	       :accessor room-thermostat)
   (name :initarg name
	 :accessor room-name)))

;;; the change-temp method simulates the result of changing
;;; the temperature in a room
(defmethod change-temp ((place room) temp-change)
  (let ((new-temp (+ (room-temp place) temp-change)))
    (setf (room-temp place) new-temp)
    (terpri)
    (prin1 "the temperature in ")
    (prin1 (room-name place))
    (prin1 " is now ")
    (prin1 new-temp)
    (terpri)
    (check-temp place)))

;;; The change-setting method simulates the effect of changing
;;; the setting of a thermostat
(defmethod change-setting ((room room) new-setting)
  (let ((therm (room-thermostat room)))
    (setf (therm-setting therm) new-setting)
    (prin1 "changing setting of thermostat in ")
    (prin1 (room-name room))
    (prin1 " to ")
    (prin1 new-setting)
    (terpri)
    (check-temp room)))

;;; check-temp examines the temperature of a room.  If it
;;; is less than the thermostat setting, it turns the heater
;;; on.  Otherwise, it turns the heater off.
(defmethod check-temp ((room room))
  (let* ((therm (room-thermostat room))
	 (heater (slot-value therm 'heater)))
    (cond ((> (therm-setting therm) (room-temp room))
	   (send-heater heater 'on))
	  (t (send-heater heater 'off)))))

;;; send-heater simulates a message to turn a heater on or off
(defmethod send-heater ((heater heater) new-state)
  (case new-state
    (on (if (equal (heater-state heater) 'off)
	    (turn-on heater))
	(heat-rooms (slot-value heater 'rooms-heated) 1))
    (off (if (eql (heater-state heater) 'on)
	     (turn-off heater)))))

(defmethod turn-on ((heater heater))
  (setf (heater-state heater) 'on)
  (prin1 "turning on heater in ")
  (prin1 (slot-value heater 'location))
  (terpri))

(defmethod turn-off ((heater heater))
  (setf (heater-state heater) 'off)
  (prin1 "turning off heater in ")
  (prin1 (slot-value heater 'location))
  (terpri))

;;; this function raises the temperature of a list of rooms
(defun heat-rooms (rooms amount)
  (cond ((null rooms) nil)
	(t (change-temp (first rooms) amount)
	   (heat-rooms (rest rooms) amount))))

;;; sample instance for running the simulation:
(defparameter *office-heater* (make-instance 'heater 'loc 'office))

(defparameter *room-325* (make-instance 'room
					'therm (make-instance
						'heater-thermostat
						'heater-obj *office-heater*)
					'name 'room-325))

(setf (slot-value *office-heater* 'rooms-heated) (list *room-325*))
