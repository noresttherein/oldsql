package com.hcore.clientapi.repository

import com.hcore.ogre.slang.matching
import com.hcore.ogre.slang.options.extensions
import matching._
import extensions._

import scala.reflect.ClassTag
import scala.slick.driver.{JdbcDriver, PostgresDriver}

//todo extract this to ogre package and remove defaults for cortb db
class DBConfig(val url :String, username :String=null, passwd :String=null, slickDriver :JdbcDriver=null, jdbcDriver :String=null) {
	val user = Option(username)
	val password = Option(passwd)
	val driver :JdbcDriver = orDefault("slickDriver", slickDriver, PostgresDriver)
	val driverClass = orDefault("jdbcDriver", jdbcDriver, "org.postgresql.Driver")

	import driver.simple._

	lazy val database = Database.forURL(url, user.orNull, password.orNull, driver = driverClass)

	private[this] def orDefault[T :ClassTag](param :String, value :T, default :T) =
		value.unless(_==null) getOrElse (url match {
			case rx"jdbc:postgresql://(.*)$_" => default
			case _ => throw new IllegalArgumentException(s"Cannot create DBConfig: $param not specified and $url is not a postgresql url")
		})

	override def toString = s"DBConfig($url, user=$user, passwd=$password, driver=${driverClass.getClass}, profile=${driver.getClass}})"
}

object DBConfig {
	def apply(url :String, username :String=null, passwd :String=null, slickDriver :JdbcDriver=null, jdbcDriver :String=null) =
		new DBConfig(url, username, passwd, slickDriver, jdbcDriver)

	val Defaults = new DBConfig("jdbc:postgresql://localhost:6543/adpilot", "adpilot", "adpilot")
}