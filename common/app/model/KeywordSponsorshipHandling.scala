package model

import common.Edition
import common.dfp.DfpAgent

case class KeywordSponsorshipHandling(
  id: String,
  adUnitSuffix: String,
  keywordIds: Seq[String]) {

  def hasPageSkin(edition: Edition): Boolean = DfpAgent.hasPageSkin(adUnitSuffix, edition)
}
