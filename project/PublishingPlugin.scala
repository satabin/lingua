import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import com.typesafe.sbt.pgp.PgpKeys._
import sbt.Keys._
import sbt.internal.util.ConsoleLogger
import sbt.librarymanagement.PublishConfiguration
import sbt.sbtpgp.Compat.publishSignedConfigurationTask
import sbt.{AutoPlugin, Package, ThisBuild}

import scala.sys.SystemProperties
import scala.util.Try

object PublishingPlugin extends AutoPlugin {

   implicit class PropsExtensions(props: SystemProperties) {
      def getBoolean(name: String, default: Boolean): Boolean = {
        Try(props.get(name).map(_.toBoolean)).toOption.flatten.getOrElse(default)
      }

      def getInt(name: String, default: Int): Int= {
        Try(props.get(name).map(_.toInt)).toOption.flatten.getOrElse(default)
      }
   }

  protected val logger: ConsoleLogger = ConsoleLogger()

  override def trigger = allRequirements

  override lazy val projectSettings = Seq(
    publishConfiguration := withOverwrite(publishConfiguration.value, isSnapshot.value)
    , publishSignedConfiguration := withOverwrite(publishSignedConfigurationTask.value, isSnapshot.value)
    , publishLocalConfiguration ~= withOverwriteEnabled
    , publishLocalSignedConfiguration ~= withOverwriteEnabled
  )

  private def withOverwriteEnabled(config: PublishConfiguration) = {
    config.withOverwrite(true)
  }

  private def withOverwrite(config: PublishConfiguration, isSnapshot: Boolean) = {
    val doOverwrite = sys.props.getBoolean("build.publish.overwrite", config.overwrite)
    // in case overwrite is already enabled (snapshots, smth else) we should not disable it
    config.withOverwrite(doOverwrite || config.overwrite || isSnapshot)
  }
}
