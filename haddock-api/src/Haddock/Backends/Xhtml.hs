--------------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mark Lentczner    2010,
--                    Mateusz Kowalczyk 2013,
--                    Nikita Churaev    2016
-- License     :  BSD-like
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

--------------------------------------------------------------------------------

module Haddock.Backends.Xhtml (
  ppHtml, copyHtmlBits,
  ppHtmlIndex, ppHtmlContents,
) where

--------------------------------------------------------------------------------

import Prelude

import Control.Monad (forM_, when, unless)
import Data.Function
import Data.Ord (comparing)
import Data.Maybe
import Data.Char (isLetter, isAscii, toUpper)
import Data.List (sortBy, nubBy, intercalate, isPrefixOf)

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map hiding (Map)
import qualified Data.Set as Set hiding (Set)

import System.FilePath (takeFileName, (</>))
import System.Directory (createDirectoryIfMissing, copyFile)

import Haddock.Backends.Xhtml.Decl
import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Layout
import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Themes
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils
import Haddock.ModuleTree
import Haddock.Types
import Haddock.Version
import Haddock.Utils
import Haddock.GhcUtils

import Text.XHtml (Html, HtmlTable, (+++), (!), (<<))
import qualified Text.XHtml as H

import DynFlags (Language (..))
import GHC hiding (NoLink, moduleInfo)
import Name
import Module

--------------------------------------------------------------------------------
-- * Generating HTML documentation
--------------------------------------------------------------------------------

ppHtml
    :: DynFlags     -- ^ GHC dynamic flags.
    -> String       -- ^ Documentation title.
    -> Maybe String -- ^ Package name, if available.
    -> [Interface]  -- ^ List of interfaces.
    -> FilePath     -- ^ Output directory.
    -> Maybe (MDoc GHC.RdrName) -- ^ Documentation prologue text, if available.
    -> Themes       -- ^ Available themes.
    -> SourceURLs   -- ^ The source URL (--source).
    -> WikiURLs     -- ^ The wiki URL (--wiki).
    -> Maybe String -- ^ The contents URL (--use-contents).
    -> Maybe String -- ^ The index URL (--use-index).
    -> Bool         -- ^ Whether to use unicode in output (--use-unicode).
    -> QualOption   -- ^ How to qualify names.
    -> Bool         -- ^ Whether to output indented HTML.
    -> IO ()
ppHtml dflags doctitle maybePackage ifaces odir prologue themes
       maybeSourceUrl maybeWikiUrl maybeContentsUrl maybeIndexUrl
       unicode qual debug = do
  let
    instIfaces    = map toInstalledIface visibleIfaces
    visibleIfaces = filter visible ifaces
    visible i     = OptHide `notElem` ifaceOptions i

  when (isNothing maybeContentsUrl) $
    ppHtmlContents dflags odir doctitle maybePackage
                   themes maybeIndexUrl maybeSourceUrl maybeWikiUrl
                   instIfaces False prologue debug (makeContentsQual qual)

  when (isNothing maybeIndexUrl) $
    ppHtmlIndex odir doctitle maybePackage themes
                maybeContentsUrl maybeSourceUrl maybeWikiUrl
                instIfaces debug

  forM_ visibleIfaces $ \iface ->
    ppHtmlModule odir doctitle maybePackage themes instIfaces
                 maybeSourceUrl maybeWikiUrl maybeContentsUrl maybeIndexUrl
                 unicode qual debug iface

--------------------------------------------------------------------------------
-- * Copying HTML resources
--------------------------------------------------------------------------------

htmlResources :: [String]
htmlResources = [
    "js.cookie.js",
    "mousetrap.min.js",
    "jquery.min.js",
    "jquery.nanoscroller.min.js",
    "haddock.js"
  ]

copyHtmlBits :: FilePath -> FilePath -> Themes -> IO ()
copyHtmlBits odir libdir themes = do
  mapM_ copyCssFile (cssFiles themes)
  mapM_ copyLibFile htmlResources
  where
    libhtmldir = libdir </> "html"
    copyCssFile f = copyFile f (odir </> takeFileName f)
    copyLibFile f = copyFile (libhtmldir </> f) (odir </> f)

--------------------------------------------------------------------------------
-- * HTML head element
--------------------------------------------------------------------------------

headHtml :: String -> Themes -> Html
headHtml doctitle themes =
  H.header << [
    H.meta ! [H.httpequiv "Content-Type", H.content "text/html; charset=UTF-8"],
    H.meta ! [H.name "viewport",
              H.content "width=device-width, initial-scale=1.0"],
    H.thetitle << doctitle,
    styleSheet themes,
    jsFile "js.cookie.js",
    jsFile "mousetrap.min.js",
    jsFile "jquery.min.js",
    jsFile "jquery.nanoscroller.min.js",
    jsFile "haddock.js",
    jsCode "window.onload = function() { haddock._initPage(); }"
  ]
  where
    jsFile src = H.script ! [H.src src, H.thetype "text/javascript"] << H.noHtml
    jsCode code = H.script ! [H.thetype "text/javascript"] <<
                    H.primHtml ("//<![CDATA[\n" ++ code ++ "\n//]]>\n")

--------------------------------------------------------------------------------
-- * HTML body element
--------------------------------------------------------------------------------

newtype BodyClasses = BodyClasses [String]

newtype ContentsTab = ContentsTab Html

--------------------------------------------------------------------------------

bodyHtml
    :: String -> Maybe String -> Maybe Interface -> [InstalledInterface]
    -> SourceURLs -> WikiURLs -> Maybe String -> Maybe String
    -> BodyClasses -> Maybe ContentsTab -> Html -> Html
bodyHtml doctitle maybePackage iface instIfaces
         maybeSourceUrl maybeWikiUrl maybeContentsUrl maybeIndexUrl
         (BodyClasses bodyClassList) maybeContentsTab pageContent =
  H.body ! [H.theclass (unwords bodyClassList)] << [
    H.thediv ! [H.identifier "page"] << [
      H.thediv ! [H.identifier "page-header"] << [
        H.h1 << "Haskell",
        pageMenu
      ],
      H.thediv ! [H.identifier "content"] << pageContent,
      H.thediv ! [H.identifier "sidebar"] << [
        H.thediv ! [H.identifier "sidebar-header"] << [
          H.thediv ! [H.identifier "sidebar-logo"] << [
            H.anchor ! [H.identifier "sidebar-logo-link",
                        H.href "https://haskell.org"] << H.noHtml
          ],
          H.thediv ! [H.identifier "sidebar-title"] << [
            H.thespan ! [H.identifier "sidebar-title-text"] << sidebarTitle
          ]
        ],
        H.thediv ! [H.identifier "sidebar-tabs"] << [
          sidebarTab "sidebar-pages-tab" << genPagesTab instIfaces,
          maybeContentsTabHtml
        ]
      ],
      H.thediv ! [H.identifier "footer"] << H.paragraph << (
        "Produced by " +++
        (H.anchor ! [H.href projectUrl] << projectName) +++
        (" version " ++ projectVersion)
      )
    ]
  ]
  where
    pageMenu = H.ulist ! [H.identifier "page-menu"] << pageMenuButtons

    pageMenuButtons =
      catMaybes [
        pageSourceButton maybeSourceUrl iface,
        pageWikiButton maybeWikiUrl (fmap ifaceMod iface)
      ] ++ [
        docContentsButton maybeContentsUrl,
        docIndexButton maybeIndexUrl
      ]

    sidebarTitle =
      case maybePackage of
        Just package -> package
        Nothing      -> doctitle

    maybeContentsTabHtml =
      case maybeContentsTab of
        Just (ContentsTab html) -> sidebarTab "sidebar-contents-tab" << html
        Nothing                 -> H.noHtml

    sidebarTab elementId html =
      H.thediv ! [H.identifier elementId,
                  H.theclass "sidebar-tab"] <<
        H.thediv ! [H.identifier (elementId ++ "-content"),
                    H.theclass "mini sidebar-tab-content"] <<
          html

--------------------------------------------------------------------------------
-- * Standard page URLs
--------------------------------------------------------------------------------

getPageSourceUrl :: SourceURLs -> Maybe Interface -> Maybe String
getPageSourceUrl (Just baseUrl, _, _, _) Nothing = Just baseUrl
getPageSourceUrl (_, Just moduleUrl, _, _) (Just iface) =
  Just (spliceURL (Just (ifaceOrigFilename iface))
                  (Just (ifaceMod iface))
                  Nothing Nothing moduleUrl)
getPageSourceUrl _ _ = Nothing

getPageWikiUrl :: WikiURLs -> Maybe Module -> Maybe String
getPageWikiUrl (Just baseUrl, _, _) Nothing = Just baseUrl
getPageWikiUrl (_, Just moduleUrl, _) (Just mdl) =
    Just (spliceURL Nothing (Just mdl) Nothing Nothing moduleUrl)
getPageWikiUrl _ _ = Nothing

--------------------------------------------------------------------------------
-- * Page menu buttons
--------------------------------------------------------------------------------

data ExistsInSidebar = NotInSidebar | ExistsInSidebar

makePageMenuButton :: String -> ExistsInSidebar -> String -> Html
makePageMenuButton label existsInSidebar url =
  H.li ! liAttrs << (H.anchor ! [H.href url] << label)
  where
    liAttrs = case existsInSidebar of
      ExistsInSidebar -> [H.theclass "exists-in-sidebar"]
      NotInSidebar    -> []

pageSourceButton :: SourceURLs -> Maybe Interface -> Maybe Html
pageSourceButton sourceUrls maybeIface =
  case getPageSourceUrl sourceUrls maybeIface of
    Just url -> Just (makePageMenuButton "Source" NotInSidebar url)
    Nothing  -> Nothing

pageWikiButton :: WikiURLs -> Maybe Module -> Maybe Html
pageWikiButton sourceUrls maybeMdl =
  case getPageWikiUrl sourceUrls maybeMdl of
    Just url -> Just (makePageMenuButton "Wiki" NotInSidebar url)
    Nothing  -> Nothing

docContentsButton :: Maybe String -> Html
docContentsButton maybeContentsUrl =
  makePageMenuButton "Contents" ExistsInSidebar url
  where
    url = fromMaybe contentsHtmlFile maybeContentsUrl

docIndexButton :: Maybe String -> Html
docIndexButton maybeIndexUrl =
  makePageMenuButton "Index" ExistsInSidebar url
  where
    url = fromMaybe indexHtmlFile maybeIndexUrl

--------------------------------------------------------------------------------
-- * Generate the documentation contents
--------------------------------------------------------------------------------

ppHtmlContents
   :: DynFlags
   -> FilePath
   -> String
   -> Maybe String
   -> Themes
   -> Maybe String
   -> SourceURLs
   -> WikiURLs
   -> [InstalledInterface] -> Bool -> Maybe (MDoc GHC.RdrName)
   -> Bool
   -> Qualification  -- ^ How to qualify names
   -> IO ()
ppHtmlContents dflags odir doctitle maybePackage themes
               maybeIndexUrl maybeSourceUrl maybeWikiUrl
               ifaces showPkgs prologue debug qual = do
  createDirectoryIfMissing True odir
  writeFile (odir </> contentsHtmlFile) (renderToString debug html)
  where
    tree = mkModuleTree
              dflags showPkgs
              [(instMod iface, toInstalledDescription iface) | iface <- ifaces]
    html =
      headHtml doctitle themes +++
      bodyHtml doctitle maybePackage Nothing ifaces
               maybeSourceUrl maybeWikiUrl Nothing maybeIndexUrl
               (BodyClasses []) Nothing << [
        genContentsPrologue qual doctitle prologue,
        genContentsModuleList qual tree
      ]

--------------------------------------------------------------------------------

genContentsPrologue
  :: Qualification -> String -> Maybe (MDoc GHC.RdrName) -> Html
genContentsPrologue _ _ Nothing = H.noHtml
genContentsPrologue qual title (Just doc) =
  H.thediv ! [H.identifier "contents-prologue"] << [
    H.h1 << title,
    docElement H.thediv (rdrDocToHtml qual doc)
  ]

--------------------------------------------------------------------------------

genContentsModuleList :: Qualification -> [ModuleTree] -> Html
genContentsModuleList qual ts =
  H.thediv ! [H.identifier "module-list"] << [
    H.h1 << "Modules",
    genModuleList qual [] "n" ts
  ]

--------------------------------------------------------------------------------

genModuleList :: Qualification -> [String] -> String -> [ModuleTree] -> Html
genModuleList qual ss p ts =
  case ts of
    [] -> H.noHtml
    _  -> H.unordList (zipWith (genModuleListNode qual ss) ps ts)
  where
    ps = [ p ++ '.' : show i | i <- [(1::Int)..]]

genModuleListNode :: Qualification -> [String] -> String -> ModuleTree -> Html
genModuleListNode qual ss p (Node s leaf pkg srcPkg short ts) =
  htmlModule <+> shortDescr +++ htmlPkg +++ subtree
  where
    modAttrs = case (ts, leaf) of
      (_:_, False) -> collapseControl p True "module"
      (_,   _    ) -> [H.theclass "module"]

    collapseButton = case (ts, leaf) of
      (_:_, True) -> H.thespan ! collapseControl p True "" << H.noHtml
      (_,   _   ) -> H.noHtml
      -- We only need an explicit collapser button when the module name
      -- is also a leaf, and so is a link to a module page. Indeed, the
      -- spaceHtml is a minor hack and does upset the layout a fraction.

    htmlModule =
      H.thespan ! modAttrs << [
        collapseButton,
        if leaf
          then ppModule (mkModule pkgKey mdlName)
          else H.toHtml s
      ]
      where
        pkgKey  = stringToPackageKey (fromMaybe "" pkg)
        mdlName = mkModuleName mdl

    mdl = intercalate "." (reverse (s:ss))

    shortDescr =
      case short of
        Just s -> H.thespan ! [H.theclass "short-descr"] << origDocToHtml qual s
        Nothing -> H.noHtml

    htmlPkg =
      case srcPkg of
        Just pkg -> H.thespan ! [H.theclass "package"] << pkg
        Nothing  -> H.noHtml

    subtree = genModuleList qual (s:ss) p ts ! collapseSection p True ""

--------------------------------------------------------------------------------
-- * Generate the Pages sidebar tab
--------------------------------------------------------------------------------

genPagesTab :: [InstalledInterface] -> Html
genPagesTab ifaces =
  H.toHtml [
    H.unordList [
      standardPageLink "Contents" "index.html",
      standardPageLink "Index" "doc-index.html"
    ],
    H.thediv ! [H.theclass "module-list"] << [
      H.h1 ! [H.theclass "mini-item"] << "Modules",
      H.ulist << map moduleLink (genTabModuleList ifaces)
    ]
  ]
  where
    moduleLink m = H.li ! [H.theclass "module"] << m
    standardPageLink label url =
      H.anchor ! [H.href url, H.theclass "mini-item", H.target "_parent"] <<
        label

--------------------------------------------------------------------------------

genTabModuleList :: [InstalledInterface] -> [Html]
genTabModuleList ifaces =
  map (\(name, mdl) -> makeLink name mdl)
    $ nubBy ((==) `on` fst)
    $ sortBy (comparing fst)
    $ mods
  where
    mods = [(moduleString mdl, mdl) | mdl <- map instMod ifaces]
    makeLink name mdl =
      H.anchor ! [H.href (moduleHtmlFile mdl), H.theclass "mini-item",
                  H.target "_parent"] <<
        name

--------------------------------------------------------------------------------
-- * Generate the index
--------------------------------------------------------------------------------

type Index = Map String (Set IndexEntry)

data IndexEntry = IndexEntry !GHC.Name !GHC.Module !IndexType !Bool
  deriving (Eq, Ord)

data IndexType = TypeOrClassIT | ConstructorIT | FunctionIT
  deriving (Eq, Ord)

--------------------------------------------------------------------------------

buildIndex :: [InstalledInterface] -> Index
buildIndex ifaces =
  Map.unionsWith Set.union (map getIfaceIndex ifaces)
  where
    getIfaceIndex :: InstalledInterface -> Index
    getIfaceIndex iface =
      makeIndex (instExports iface)
      where
        makeIndex [] = Map.empty
        makeIndex (name:xs) =
          insertIndexEntry (occNameString (nameOccName name))
                           (makeEntry name) (makeIndex xs)
        makeEntry name =
          IndexEntry name mdl (getEntryType name) (Set.member name visible)
        visible = Set.fromList (instVisibleExports iface)
        mdl = instMod iface

    insertIndexEntry :: String -> IndexEntry -> Index -> Index
    insertIndexEntry name entry index =
      Map.insert name (Set.insert entry existingEntries) index
      where
        existingEntries = case Map.lookup name index of
          Just entries -> entries
          Nothing      -> Set.empty

    getEntryType name
      | not (isValOcc on) = TypeOrClassIT
      | isDataOcc on      = ConstructorIT
      | otherwise         = FunctionIT
      where on = nameOccName name

--------------------------------------------------------------------------------

ppHtmlIndex
    :: FilePath -> String -> Maybe String -> Themes
    -> Maybe String -> SourceURLs -> WikiURLs
    -> [InstalledInterface] -> Bool -> IO ()
ppHtmlIndex odir doctitle maybePackage themes
            maybeContentsUrl maybeSourceUrl maybeWikiUrl
            ifaces debug = do
  createDirectoryIfMissing True odir
  writeFile (odir </> indexHtmlFile)
            (renderToString debug mainPageHtml)
  when doLetterPages $ do
    let allHtml = makeIndexPage True Nothing allIndex
    mapM_ writeLetterIndex alphabetChars
    writeFile (odir </> subIndexHtmlFile allName)
              (renderToString debug allHtml)
    when (not (null otherIndex)) $ do
      let otherHtml = makeIndexPage True Nothing otherIndex
      writeFile (odir </> subIndexHtmlFile otherName)
                (renderToString debug otherHtml)
  where
    mainPageHtml = makeIndexPage doLetterPages Nothing
                                 (if doLetterPages then [] else allIndex)

    indexName ch = "Index" ++ maybe "" (\c -> " - " ++ [c]) ch

    makeIndexPage showAlphabet ch items =
      headHtml (doctitle ++ " (" ++ indexName ch ++ ")") themes +++
      bodyHtml doctitle maybePackage Nothing ifaces
               maybeSourceUrl maybeWikiUrl maybeContentsUrl Nothing
               (BodyClasses []) Nothing << [
        if showAlphabet then alphabetHtml else H.noHtml,
        H.thediv ! [H.identifier "index"] <<
          if null items
            then [H.p ! [H.theclass "no-items"] << "Select a letter."]
            else [H.h1 << indexName ch, genIndexTable items]
      ]

    -- Basic indices
    ----------------

    allIndex =
      sortBy cmp (Map.toList (buildIndex ifaces))
      where
        cmp (n1,_) (n2,_) = comparing (map toUpper) n1 n2

    otherIndex =
      filter func allIndex
      where
        func (name,_) = Set.notMember (toUpper (head name)) alphabetCharSet

    doLetterPages = length allIndex > 150

    writeLetterIndex ch =
      unless (null items) $
        writeFile (odir </> subIndexHtmlFile [ch])
                  (renderToString debug html)
      where
        html = makeIndexPage True (Just ch) items
        items = filter filterFunc allIndex
        filterFunc (name,_) = toUpper (head name) == ch

    -- Alphabet
    -----------

    alphabetHtml =
      H.thediv ! [H.identifier "alphabet"] <<
        H.unordList (map makeLink linkNames)
      where
        makeLink name =
          H.anchor ! ([H.href (subIndexHtmlFile name)] ++ nonAsciiClass) << name
          where
            nonAsciiClass =
              if all isAscii name then [] else [H.theclass "non-ascii"]
        linkNames =
          map (\c -> [c]) alphabetChars ++
          if haveOtherChars then [otherName] else [] ++
          [allName]

    allName   = "All"
    otherName = "Other"

    basicSpecials = ":!#$%&*+./<=>?@\\^|-~_"

    initialCharSet =
      Set.fromList (map (\(name,_) -> toUpper (head name)) allIndex)

    initialLetterSet = Set.filter isLetter initialCharSet

    alphabetChars =
      Set.toAscList initialLetterSet ++
      filter (\ch -> Set.member ch initialCharSet) basicSpecials

    alphabetCharSet = Set.fromList alphabetChars

    haveOtherChars = not (initialLetterSet == alphabetCharSet)

    -- Index table
    --------------

    genIndexTable items = H.table << H.aboves (concatMap genIndexEntry items)

    genIndexEntry (name, entrySet) =
      if allSame (map getEntryType entryList)
        then [H.besides [nameTd, genLinksTd entryList]]
        else
          [H.besides [nameTd, H.td << H.noHtml]] ++
          genCategorizedEntries entryList
      where
        entryList = Set.toAscList entrySet
        nameTd = H.td ! [H.theclass "src"] << name

    genCategorizedEntries entryList =
      catMaybes [
        makeRow "Type/Class"  (filterByType TypeOrClassIT),
        makeRow "Constructor" (filterByType ConstructorIT),
        makeRow "Function"    (filterByType FunctionIT)
      ]
      where
        filterByType t = filter (\e -> getEntryType e == t) entryList
        makeRow _ [] = Nothing
        makeRow rowName rowEntries =
          Just (H.besides [
            H.td ! [H.theclass "cat"] << rowName,
            genLinksTd rowEntries
          ])

    genLinksTd entryList =
      H.td ! [H.theclass "modules"] << links
      where
        links = hsep (punctuate comma (map makeLink entryList))
        makeLink (IndexEntry name mdl _ visible) =
          if visible
            then linkId mdl (Just name) << moduleString mdl
            else H.toHtml (moduleString mdl)

    -- Utilities
    ------------

    getEntryType (IndexEntry _ _ t _) = t

--------------------------------------------------------------------------------
-- * Generate module documentation
--------------------------------------------------------------------------------

ppHtmlModule
    :: FilePath -> String -> Maybe String -> Themes -> [InstalledInterface]
    -> SourceURLs -> WikiURLs -> Maybe String -> Maybe String
    -> Bool -> QualOption -> Bool -> Interface -> IO ()
ppHtmlModule odir doctitle maybePackage themes instIfaces
             maybeSourceUrl maybeWikiUrl maybeContentsUrl maybeIndexUrl
             unicode qual debug iface = do
  createDirectoryIfMissing True odir
  writeFile (odir </> moduleHtmlFile mdl) (renderToString debug html)
  where
    mdl      = ifaceMod iface
    aliases  = ifaceModuleAliases iface
    mdlName  = moduleString mdl
    realQual = makeModuleQual qual aliases mdl
    exports  = numberSectionHeadings (ifaceRnExportItems iface)
    ctsTab   = genModuleContentsTab mdl iface unicode realQual
    html =
      headHtml mdlName themes +++
      bodyHtml mdlName maybePackage (Just iface) instIfaces
               maybeSourceUrl maybeWikiUrl maybeContentsUrl maybeIndexUrl
               (BodyClasses ["has-module-prologue"])
               (Just (ContentsTab ctsTab)) << [
        H.h1 ! [H.theclass "module-name"] << mdlName,
        H.thediv ! [H.identifier "module-prologue"] << [
          genModuleInfo iface,
          genModuleContents realQual exports
        ],
        genInterfaceDocs maybeSourceUrl maybeWikiUrl iface unicode realQual
      ]

--------------------------------------------------------------------------------

genModuleInfo :: Interface -> Html
genModuleInfo iface
  | null entries = H.noHtml
  | otherwise    = infoDiv
  where
    infoDiv =
      H.thediv ! [H.identifier "module-info"] << [
        H.h3 << "Information",
        H.table ! [H.theclass "info"] << H.aboves entries
      ]

    info = ifaceInfo iface

    entries =
      mapMaybe genEntry [
        ("Copyright",    hmi_copyright),
        ("License",      hmi_license),
        ("Maintainer",   hmi_maintainer),
        ("Stability",    hmi_stability),
        ("Portability",  hmi_portability),
        ("Safe Haskell", hmi_safety),
        ("Language",     getDialectName)
      ] ++
      extensionsEntry

    genEntry (name, getHtml) =
      case getHtml info of
        Just html -> Just (H.besides [H.th << name, H.td << html])
        Nothing   -> Nothing

    getDialectName inf =
      case hmi_language inf of
        Nothing          -> Nothing
        Just Haskell98   -> Just "Haskell98"
        Just Haskell2010 -> Just "Haskell2010"

    extensionsEntry =
      if elem OptShowExtensions (ifaceOptions iface)
        then
          let extNames = map (dropOpt . show) (hmi_extensions info)
          in case map H.toHtml extNames of
            []  -> []
            [x] -> extField x
            xs  -> extField (H.unordList xs ! [H.theclass "extension-list"])
        else []
      where
        extField html = [H.besides [H.th << "Extensions", H.td << html]]
        dropOpt x = if "Opt_" `isPrefixOf` x then drop 4 x else x

--------------------------------------------------------------------------------

genModuleContents :: Qualification -> [ExportItem DocName] -> Html
genModuleContents qual exports
  | null sections = H.noHtml
  | otherwise     = contentsDiv
  where
    contentsDiv =
      H.thediv ! [H.identifier "table-of-contents"] << [
        H.h3 << "Contents",
        H.unordList sections
      ]

    (sections, _) = process 0 exports

    process :: Int -> [ExportItem DocName] -> ([Html], [ExportItem DocName])
    process _ [] = ([], [])
    process n items@((ExportGroup level gid doc):rest)
      | level <= n = ([], items)
      | otherwise  = (html:subSecs, rest2)
      where
        html =
          H.anchor ! [H.href (groupId gid)] << [
            docToHtmlNoAnchors (Just gid) qual (mkMeta doc),
            mkSubsections subSecs
          ]
        (subSecs, rest1) = process level rest
        (secs,    rest2) = process n     rest1
    process n (_:rest) = process n rest

    mkSubsections [] = H.noHtml
    mkSubsections ss = H.unordList ss

-- we need to assign a unique id to each section heading so we can hyperlink
-- them from the contents:
numberSectionHeadings :: [ExportItem DocName] -> [ExportItem DocName]
numberSectionHeadings = go 1
  where go :: Int -> [ExportItem DocName] -> [ExportItem DocName]
        go _ [] = []
        go n (ExportGroup lev _ doc : es)
          = ExportGroup lev (show n) doc : go (n+1) es
        go n (other:es)
          = other : go n es

--------------------------------------------------------------------------------

genInterfaceDocs
    :: SourceURLs -> WikiURLs -> Interface -> Bool -> Qualification -> Html
genInterfaceDocs maybeSourceUrl maybeWikiUrl iface unicode qual =
  H.toHtml [
    description,
    synopsis,
    H.thediv ! [H.identifier "interface"] << [
      maybeDocHeader,
      docBody
    ]
  ]
  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)

    description
      | H.isNoHtml doc = doc
      | otherwise =
          H.thediv ! [H.identifier "description"] << [
            H.h1 << "Description",
            doc
          ]
      where
        doc = docSection Nothing qual (ifaceRnDoc iface)

    -- Omit the synopsis if there are no documentation annotations at all
    synopsis
      | shouldOmitSynopsis exports = H.noHtml
      | otherwise =
          H.thediv ! [H.identifier "synopsis"] << (
            H.h1 ! collapseControl "syn" False "" << "Synopsis" +++
            shortDeclList (
              mapMaybe (processExport True linksInfo unicode qual) exports
            ) ! (collapseSection "syn" False "" ++ collapseToggle "syn")
          )

    -- If the documentation doesn't begin with a section header, then add one
    -- ("Documentation").
    maybeDocHeader = case exports of
        []                -> H.noHtml
        ExportGroup{} : _ -> H.noHtml
        _                 -> H.h1 << "Documentation"

    docBody = H.toHtml processedExports

    processedExports =
      mapMaybe (processExport False linksInfo unicode qual) exports

    linksInfo = (maybeSourceUrl, maybeWikiUrl)

--------------------------------------------------------------------------------

processExport
  :: Bool -> LinksInfo -> Bool -> Qualification
  -> ExportItem DocName -> Maybe Html
processExport summary links unicode qual item = case item of
  ExportDecl {expItemDecl = L _ (InstD _)} ->
    Nothing -- Hide empty instances.
  ExportGroup lev id0 doc ->
    nothingIf summary (
      groupHeading lev id0 << docToHtml (Just id0) qual (mkMeta doc)
    )
  ExportDecl decl doc subdocs insts fixities splice ->
    processDecl summary (
      ppDecl summary links decl doc insts fixities subdocs splice unicode qual
    )
  ExportNoDecl y [] ->
    processDeclOneLiner summary (ppDocName qual Prefix True y)
  ExportNoDecl y subs ->
    processDeclOneLiner summary (
      ppDocName qual Prefix True y +++
      parenList (map (ppDocName qual Prefix True) subs)
    )
  ExportDoc doc ->
    nothingIf summary (docSection_ Nothing qual doc)
  ExportModule mdl ->
    processDeclOneLiner summary (H.toHtml "module" <+> ppModule mdl)

processDecl :: Bool -> Html -> Maybe Html
processDecl True = Just
processDecl False = Just . divTopDecl

processDeclOneLiner :: Bool -> Html -> Maybe Html
processDeclOneLiner True = Just
processDeclOneLiner False = Just . divTopDecl . declElem

groupHeading :: Int -> String -> Html -> Html
groupHeading lev id0 = groupTag lev ! [H.identifier (groupId id0)]

groupTag :: Int -> Html -> Html
groupTag level
  | level == 1 = H.h1
  | level == 2 = H.h2
  | level == 3 = H.h3
  | otherwise  = H.h4

--------------------------------------------------------------------------------
-- * Module contents tab
--------------------------------------------------------------------------------

genModuleContentsTab :: Module -> Interface -> Bool -> Qualification -> Html
genModuleContentsTab mdl iface unicode qual =
  H.thediv ! [H.theclass "contents module"] <<
    H.ulist << (standardPages ++ members)
  where
    standardPages = [
        standardPage "#description" "Description",
        if not (shouldOmitSynopsis exports)
          then standardPage "#synopsis" "Synopsis"
          else H.noHtml
      ]
    standardPage href name =
      H.li ! [H.theclass "std"] << H.h1 <<
        H.anchor ! [H.theclass "mini-item", H.href href] << name
    exports = numberSectionHeadings (ifaceRnExportItems iface)
    members = processForModuleContentsTab mdl unicode qual exports

--------------------------------------------------------------------------------

processForModuleContentsTab
    :: Module -> Bool -> Qualification -> [ExportItem DocName] -> [Html]
processForModuleContentsTab mdl unicode qual items =
  let (result, _) = process 0 items in result
  where
    process :: Int -> [ExportItem DocName] -> ([Html], [ExportItem DocName])
    process _ [] = ([], [])
    process level (item:remaining) = case item of
      ExportDecl { expItemDecl = L _ itemDecl } ->
        (fmap resultToHtml results ++ remainingMembers, remainingAfterThis)
        where
          (remainingMembers, remainingAfterThis) =
             process level remaining
          resultToHtml (html, url) =
            H.li ! [H.theclass "top"] <<
              H.anchor ! [H.theclass "mini-item top", H.href url] << html
          results = processExportItem itemDecl
      ExportGroup level' groupId title ->
        if level' > level
          then -- This is a child group.
            let
              hash = "#g:" ++ groupId
              titleHtml = docToHtml Nothing qual (mkMeta title)
              childLiClasses = unwords (
                  ["group"] ++
                  if not (null childMembers) then [] else ["no-items"]
                )
              childLi =
                H.li ! [H.theclass childLiClasses] << [
                  groupTag level <<
                    H.anchor ! [H.theclass "mini-item", H.href hash] << titleHtml,
                  H.ulist ! [H.theclass "members"] << childMembers
                ]
              (childMembers, remainingAfterChild) =
                process level' remaining
              (remainingMembers, remainingAfterThis) =
                process level remainingAfterChild
            in
              (childLi:remainingMembers, remainingAfterThis)
          else -- This is another group of the same/greater level.
            ([], item:remaining)
      _ -> process level remaining

    processExportItem itemDecl = case itemDecl of
      TyClD d ->
        [(html, url)]
        where
          html = case d of
            (FamDecl   decl) -> [ppTyFamHeader True False decl unicode qual]
            (DataDecl  {})   -> [keyword "data" <+> b]
            (SynDecl   {})   -> [keyword "type" <+> b]
            (ClassDecl {})   -> [keyword "class" <+> b]
          url = moduleNameUrl mdl (nameOccName (getName (tcdName d)))
          b = ppTyClBinderWithVarsMini mdl d
      SigD (TypeSig lnames (L _ _) _) ->
        map process lnames
        where
          process lname = (html, url)
            where
              occName = nameOccName (getName (unLoc lname))
              html    = [ppNameMini Prefix mdl occName]
              url     = moduleNameUrl mdl occName
      _ -> []

--------------------------------------------------------------------------------

ppNameMini :: Notation -> Module -> OccName -> Html
ppNameMini notation mdl nm =
  H.thespan ! [H.theclass "main-name"] << ppBinder' notation nm

ppParamNameMini :: Name -> Html
ppParamNameMini nm =
  H.thespan ! [H.theclass "param"] << ppTyName nm

ppTyClBinderWithVarsMini :: Module -> TyClDecl DocName -> Html
ppTyClBinderWithVarsMini mdl decl =
  ppTypeApp n [] ns func ppParamNameMini
  where
    n = tcdName decl
    ns = tyvarNames (tcdTyVars decl)
    func isInfix = ppNameMini isInfix mdl . nameOccName . getName

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

nothingIf :: Bool -> a -> Maybe a
nothingIf True _ = Nothing
nothingIf False a = Just a

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs

-- TODO: if something has only sub-docs, or fn-args-docs, should it be measured
-- here and thus prevent omitting the synopsis?
exportHasDoc :: ExportItem n -> Bool
exportHasDoc ExportDecl {expItemMbDoc = (Documentation mDoc mWarning, _)} =
  isJust mDoc || isJust mWarning
exportHasDoc (ExportNoDecl _ _) = False
exportHasDoc (ExportModule _) = False
exportHasDoc _ = False

noDocumentedExports :: [ExportItem n] -> Bool
noDocumentedExports = not . any exportHasDoc

shouldOmitSynopsis :: [ExportItem n] -> Bool
shouldOmitSynopsis = noDocumentedExports

--------------------------------------------------------------------------------
