-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html.Layout
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.Layout (
  shortDeclList,
  shortSubDecls,

  divTopDecl,

  SubDecl,
  subArguments,
  subAssociatedTypes,
  subConstructors,
  subEquations,
  subFields,
  subInstances, subInstHead, subInstDetails,
  subMethods,
  subMinimal,

  topDeclElem, declElem,
) where


import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils
import Haddock.Types
import Haddock.Utils (makeAnchorId)
import qualified Data.Map as Map
import Text.XHtml hiding ( name, title, p, quote )
import qualified Text.XHtml as H

import FastString            ( unpackFS )
import GHC

--------------------------------------------------------------------------------
-- * Declaration containers
--------------------------------------------------------------------------------


shortDeclList :: [Html] -> Html
shortDeclList items = ulist << map (li ! [theclass "src short"] <<) items


shortSubDecls :: Bool -> [Html] -> Html
shortSubDecls inst items = ulist ! [theclass c] << map (i <<) items
  where i | inst      = li ! [theclass "inst"]
          | otherwise = li
        c | inst      = "inst"
          | otherwise = "subs"


divTopDecl :: Html -> Html
divTopDecl = thediv ! [theclass "top"]


type SubDecl = (Html, Maybe (MDoc DocName), [Html])


divSubDecls :: (HTML a) => String -> a -> Maybe Html -> Html
divSubDecls cssClass captionName = maybe noHtml wrap
  where
    wrap = (subSection <<) . (subCaption +++)
    subSection = thediv ! [theclass $ unwords ["subs", cssClass]]
    subCaption = h4 << captionName


subDlist :: Qualification -> [SubDecl] -> Maybe Html
subDlist _ [] = Nothing
subDlist qual decls = Just $ ulist << map subEntry decls
  where
    subEntry (decl, mdoc, subs) =
      li <<
        (define ! [theclass "src"] << decl +++
         docElement thediv << (fmap (docToHtml Nothing qual) mdoc) +++
         subs)


subTable :: Qualification -> [SubDecl] -> Maybe Html
subTable _ [] = Nothing
subTable qual decls = Just $ table << aboves (concatMap subRow decls)
  where
    subRow (decl, mdoc, subs) =
      (td ! [theclass "src"] << decl
       <->
       docElement td << fmap (docToHtml Nothing qual) mdoc)
      : map (cell . (td <<)) subs


-- | Sub table with source information (optional).
subTableSrc :: Qualification -> LinksInfo -> Bool -> [(SubDecl,Located DocName)] -> Maybe Html
subTableSrc _ _  _ [] = Nothing
subTableSrc qual lnks splice decls = Just $ table << aboves (concatMap subRow decls)
  where
    subRow ((decl, mdoc, subs),L loc dn) =
      (td ! [theclass "src clearfix"] <<
        (thespan ! [theclass "inst-body"] << decl)
        <+> (thespan ! [theclass "inst-links"] << linkHtml loc dn)
      <->
      docElement td << fmap (docToHtml Nothing qual) mdoc
      )
      : map (cell . (td <<)) subs
    linkHtml loc@(RealSrcSpan _) dn = links lnks loc splice dn
    linkHtml _ _ = noHtml

subBlock :: [Html] -> Maybe Html
subBlock [] = Nothing
subBlock hs = Just $ toHtml hs


subArguments :: Qualification -> [SubDecl] -> Html
subArguments qual = divSubDecls "arguments" "Arguments" . subTable qual


subAssociatedTypes :: [Html] -> Html
subAssociatedTypes = divSubDecls "associated-types" "Associated Types" . subBlock


subConstructors :: Qualification -> [SubDecl] -> Html
subConstructors qual = divSubDecls "constructors" "Constructors" . subDlist qual


subFields :: Qualification -> [SubDecl] -> Html
subFields qual = divSubDecls "fields" "Fields" . subDlist qual


subEquations :: Qualification -> [SubDecl] -> Html
subEquations qual = divSubDecls "equations" "Equations" . subTable qual


-- | Generate sub table for instance declarations, with source
subInstances :: Qualification
             -> String -- ^ Class name, used for anchor generation
             -> LinksInfo -> Bool
             -> [(SubDecl,Located DocName)] -> Html
subInstances qual nm lnks splice = maybe noHtml wrap . instTable
  where
    wrap = (subSection <<) . (subCaption +++)
    instTable = fmap (thediv ! collapseSection id_ True [] <<) . subTableSrc qual lnks splice
    subSection = thediv ! [theclass "subs instances"]
    subCaption = h4 ! collapseControl id_ True "caption" << "Instances"
    id_ = makeAnchorId $ "i:" ++ nm


subInstHead :: String -- ^ Instance unique id (for anchor generation)
            -> Html -- ^ Header content (instance name and type)
            -> Html
subInstHead iid hdr =
    expander noHtml <+> hdr
  where
    expander = thespan ! collapseControl (instAnchorId iid) False "instance"


subInstDetails :: String -- ^ Instance unique id (for anchor generation)
               -> [Html] -- ^ Associated type contents
               -> [Html] -- ^ Method contents (pretty-printed signatures)
               -> Html
subInstDetails iid ats mets =
    section << (subAssociatedTypes ats <+> subMethods mets)
  where
    section = thediv ! collapseSection (instAnchorId iid) False "inst-details"


instAnchorId :: String -> String
instAnchorId iid = makeAnchorId $ "i:" ++ iid


subMethods :: [Html] -> Html
subMethods = divSubDecls "methods" "Methods" . subBlock

subMinimal :: Html -> Html
subMinimal = divSubDecls "minimal" "Minimal complete definition" . Just . declElem


-- a box for displaying code
declElem :: Html -> Html
declElem = paragraph ! [theclass "src"]


-- a box for top level documented names
-- it adds a source and wiki link at the right hand side of the box
topDeclElem :: LinksInfo -> SrcSpan -> Bool -> [DocName] -> Html -> Html
topDeclElem lnks loc splice names html =
    declElem << (
      (thespan ! [theclass "decl-body"] << html) <+>
      (thespan ! [theclass "decl-links"] << links lnks loc splice (head names)))
      -- FIXME: is it ok to simply take the first name?

-- | Adds a source and wiki link at the right hand side of the box.
-- Name must be documented, otherwise we wouldn't get here.
links :: LinksInfo -> SrcSpan -> Bool -> DocName -> Html
links ((_,_,sourceMap,lineMap), (_,_,maybe_wiki_url)) loc splice (Documented n mdl) =
  (srcLink <+> wikiLink)
  where srcLink = let nameUrl = Map.lookup origPkg sourceMap
                      lineUrl = Map.lookup origPkg lineMap
                      mUrl | splice    = lineUrl
                                        -- Use the lineUrl as a backup
                           | otherwise = maybe lineUrl Just nameUrl in
          case mUrl of
            Nothing  -> noHtml
            Just url -> let url' = spliceURL (Just fname) (Just origMod)
                                               (Just n) (Just loc) url
                          in anchor ! [href url', theclass "link"] << "Source"

        wikiLink =
          case maybe_wiki_url of
            Nothing  -> noHtml
            Just url -> let url' = spliceURL (Just fname) (Just mdl)
                                               (Just n) (Just loc) url
                          in anchor ! [href url', theclass "link"] << "Comments"

        -- For source links, we want to point to the original module,
        -- because only that will have the source.
        -- TODO: do something about type instances. They will point to
        -- the module defining the type family, which is wrong.
        origMod = nameModule n
        origPkg = modulePackageKey origMod

        fname = case loc of
          RealSrcSpan l -> unpackFS (srcSpanFile l)
          UnhelpfulSpan _ -> error "links: UnhelpfulSpan"
links _ _ _ _ = noHtml
