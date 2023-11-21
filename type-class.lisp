(in-package #:org.shirakumo.graphql)

(defclass type-class (standard-class)
  ()
  (:documentation "Metaclass of classes denoting types"))

(defmethod validate-superclass ((class type-class) (super standard-class)) t)

(defmethod validate-superclass ((class standard-class) (super type-class)) nil)
