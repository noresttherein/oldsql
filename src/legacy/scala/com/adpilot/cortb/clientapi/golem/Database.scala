package com.adpilot.cortb.clientapi.golem

trait Database {
	type SessionFactory <: Database.SessionFactory
}



object Database {
	import java.sql.{Connection, DriverManager}
	import java.util.Properties

	trait SessionFactory {
		trait Session extends AutoCloseable {
			def connection :Connection

			def close :Unit = {
				connection.close()
			}
		}

		def newSession :Session
	}


	class DirectConnectionSessionFactory(driverClass :String, uri :String, properties :(String, String)*)
		extends SessionFactory
	{
		def this(driverClass :String, uri :String, user :Option[String]=None, password :Option[String]=None) =
			this(driverClass, uri, user.map("user"->) ++: password.map("password"->).toSeq :_*)

		val s :Seq[Seq[Seq[Int]]] = ???

		s.flatMap(x=>s.flatMap(y=>y.flatMap(z => z.map(_+1))))
		for (x<-s; y<- x; z<-y) yield z*z //{ val q=z+1; q*q }


		Class.forName(driverClass)

		class Session private[DirectConnectionSessionFactory] (val connection :Connection) extends super.Session

		override def newSession: Session = new Session(createConnection)

		private def createConnection = {
			val props = new Properties()
			properties.foreach{ case (name, value) => props.put(name, value) }
			DriverManager.getConnection(uri, props)

		}
	}
}