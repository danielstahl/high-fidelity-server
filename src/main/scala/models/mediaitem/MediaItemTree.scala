package models.mediaitem

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.DefaultJsonProtocol


case class Type(slug: String, name: String)

case class Tag(slugs: String, name: String, theType: Type)

case class TreeLink(tag: Tag, typeTree: Option[String])

case class Children(theType: Type, typeTree: String, children: Seq[Tag])

case class BreadCrumbs(links: Seq[Tag], treeType: String)

case class DecoratedMediaItem(slugs: String, name: String, types: Seq[Type], uris: Map[String, Seq[String]], tags: Map[String, Seq[TreeLink]])

case class MediaItemTree(mediaItem: DecoratedMediaItem,
                         breadCrumbs: BreadCrumbs,
                         children: Option[Children])

trait MediaItemTreeJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val typeFormat = jsonFormat2(Type)
  implicit val tagFormat = jsonFormat3(Tag)
  implicit val treeLinkFormat = jsonFormat2(TreeLink)
  implicit val childrenFormat = jsonFormat3(Children)
  implicit val breadCrumbsFormat = jsonFormat2(BreadCrumbs)
  implicit val decoratedMediaItemFormat = jsonFormat5(DecoratedMediaItem)
  implicit val mediaItemTreeFormat = jsonFormat3(MediaItemTree)
}

object MediaItemTreeService {

  /*
  genre -> era -> composer -> (
    album
    piece(form) -> recording
  )
  * */
  val typeTrees = Map(
    "composer" -> Seq("genre", "era", "composer", "piece", "recording"),
    "artist" -> Seq("genre", "instrument", "artist", "album")
  )

  private def toType(typeDescription: TypeDescription): Type = {
    Type(typeDescription.slug, typeDescription.name)
  }

  private def makeTreeLink(userMediaItems: Map[String, MediaItem], tagName: String, tagValue: String, typeTree: String): TreeLink = {
    val preferedTree = typeTrees(typeTree)

    val choosenTypeTree: Option[String] = if(preferedTree.contains(tagName)) Option(typeTree) else
      typeTrees
        .filter { case (key, tree) => key != typeTree}
        .find { case (key, tree) => tree.contains(tagName)}
        .map { case (key, tree) => key}

    val typeDescription = Database.typeDescriptions(tagName)

    val mediaItem = userMediaItems(tagValue)

    TreeLink(Tag(tagValue, mediaItem.name, toType(typeDescription)), choosenTypeTree)
  }

  def decorate(userMediaItems: Map[String, MediaItem], mediaItem: MediaItem, typeTree: String): DecoratedMediaItem = {
    DecoratedMediaItem(
      slugs = mediaItem.slugs,
      name = mediaItem.name,
      types = mediaItem.types.map(
        theType =>
          toType(Database.typeDescriptions(theType))),
      uris = mediaItem.uris,
      tags = mediaItem.tags.map {
        case (tagName, tagValues) =>
          tagName -> tagValues.map(tagValue =>
            makeTreeLink(userMediaItems, tagName, tagValue, typeTree))}
    )
  }

  def mediaItemTree(userMediaItems: Map[String, MediaItem], slugs: String, theType: String, theTypeTree: String): MediaItemTree = {
    val typeTree = typeTrees(theTypeTree)
    val currenTypeTreeIndex = typeTree.indexOf(theType)
    val mediaItem = userMediaItems(slugs)
    val decoratedMediaItem = decorate(userMediaItems, mediaItem, theTypeTree)

    val breadCrumbsTypes = typeTree.slice(0, typeTree.indexOf(theType))
    val breadCrumbsLinks = breadCrumbsTypes
        .map(crumbItemType =>
          userMediaItems.values.find(
            crumbCandidate =>
              mediaItem.getTag(crumbItemType).contains(crumbCandidate.slugs) &&
                crumbCandidate.types.contains(crumbItemType))
            .map(crumbItem =>
              Tag(crumbItem.slugs, crumbItem.name, Type(crumbItemType, Database.typeDescriptions(crumbItemType).name))))
          .map(optionalCrumb => optionalCrumb.orNull)
            .filter(tag => tag != null)
    val breadCrumbs = BreadCrumbs(breadCrumbsLinks, theTypeTree)

    val optionalChidrenType =
      if(typeTree.size > currenTypeTreeIndex + 1)
        Some(toType(Database.typeDescriptions(typeTree(currenTypeTreeIndex + 1))))
      else None


    val childrenItems = optionalChidrenType match {
      case Some(childrenType) =>
        userMediaItems.values
            .filter(childItem =>
              childItem.getTag(theType).contains(slugs) &&
                childItem.types.contains(childrenType.slug))
              .map(childItem =>
                Tag(childItem.slugs, childItem.name, childrenType))
      case None => Seq.empty
    }

    val optionalChildren = optionalChidrenType.map(
      childrenType => Children(childrenType, theTypeTree, childrenItems.toSeq))

    MediaItemTree(decoratedMediaItem, breadCrumbs, optionalChildren)
  }
}