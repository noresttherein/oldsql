package com.hcore.clientapi

import com.hcore.clientapi.entities.Model.Id
import com.hcore.clientapi.repository.DBConfig
import com.typesafe.config.ConfigFactory

import scala.util.Try


/**
 * Holds service configuration settings.
 */
trait Configuration {
	import scala.collection.JavaConverters._
	/**
	 * Application config object.
	 */
	val config = ConfigFactory.load()

	def configString(param :String, default : =>String) = Try(config.getString(param)).getOrElse(default)

	/** Host name/address to start service on. */
	lazy val serviceHost = Try(config.getString("service.host")).getOrElse("localhost")

	/** Port to start service on. */
	lazy val servicePort = Try(config.getInt("service.port")).getOrElse(8080)

	lazy val databaseURL = configString("database.url", DBConfig.Defaults.url)

	lazy val databaseUser = configString("database.user", DBConfig.Defaults.user.orNull)

	lazy val databasePassword = configString("database.password", DBConfig.Defaults.password.orNull)

	lazy val databaseConfig = DBConfig(databaseURL, databaseUser, databasePassword)

	lazy val blockedClients = Try(config.getLongList("clientapi.validation.blocked-clients").asScala.toSeq.map(id => Id(id:Long))).getOrElse(Seq())

}