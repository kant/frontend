package targeting

import common._
import com.gu.targeting.client.CampaignCache
import com.amazonaws.services.s3.AmazonS3Client
import conf.Configuration
import services.AwsEndpoints

import scala.util.control.NonFatal

object CampaignAgent extends Logging with ExecutionContexts {
  val crossAccountClient: AmazonS3Client = {
    val client = new AmazonS3Client(Configuration.targeting.crossAccountMandatoryCredentials)
    client.setEndpoint(AwsEndpoints.s3)
    client
  }

  private val agent = AkkaAgent[CampaignCache](new CampaignCache())

  def refresh() {
    agent.alter { old =>
      old.update(crossAccountClient, Configuration.targeting.campaignsBucket)
      old
    }
  }

  def getCampaignsForTags(tags: Seq[String]) = try {
    agent().getCampaignsForTags(tags)
  } catch {
    case NonFatal(e) => List() // TODO log here!
  }
}

