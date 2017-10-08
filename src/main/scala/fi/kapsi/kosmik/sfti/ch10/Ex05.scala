package fi.kapsi.kosmik.sfti.ch10

import java.beans.PropertyChangeListener

/**
  * The JavaBeans specification has the notion of a property change listener, a
  * standardized way for beans to communicate changes in their properties. The
  * PropertyChangeSupport class is provided as a convenience superclass for any bean
  * that wishes to support property change listeners. Unfortunately, a class that
  * already has another superclass—such as JComponent —must reimplement
  * the methods. Reimplement PropertyChangeSupport as a trait, and mix it into the
  * java.awt.Point class.
  */
object Ex05 {

  /**
    * NOTE: Provides only a small subset (as an example) of the features of java.beans.PropertyChangeSupport.
    */
  trait PropertyChangeSupport {
    private val pcs = new java.beans.PropertyChangeSupport(this)

    def addPropertyChangeListener(propertyName: String, listener: PropertyChangeListener): Unit = {
      pcs.addPropertyChangeListener(propertyName, listener)
    }

    def firePropertyChange(propertyName: String, oldValue: Any, newValue: Any): Unit = {
      pcs.firePropertyChange(propertyName, oldValue, newValue)
    }
  }


  class PointBean extends java.awt.Point with PropertyChangeSupport {
    // As an example, add support for property changes for one property
    override def setLocation(x: Int, y: Int): Unit = {
      val oldValue = getLocation
      super.setLocation(x, y)
      firePropertyChange("location", oldValue, getLocation)
    }
  }

}
