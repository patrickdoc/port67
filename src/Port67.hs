{-# LANGUAGE OverloadedStrings #-}
module Port67 where

import Text.Pandoc
import Text.Pandoc.Options
import Text.Pandoc.Walk

import qualified Data.Text  as T

port67Reader :: ReaderOptions -> T.Text -> PandocIO Pandoc
port67Reader = readMarkdown

port67Writer :: WriterOptions -> Pandoc -> PandocIO T.Text
port67Writer opts doc = writeHtml5String opts (htmlPreProcessor doc)
--port67Writer opts doc = writeNative opts (htmlPreProcessor doc)

htmlPreProcessor :: Pandoc -> Pandoc
htmlPreProcessor = walk breakSentences

interSentenceSpace :: Inline
interSentenceSpace = RawInline (Format "html") " "

sentenceStart :: Inline
sentenceStart = RawInline (Format "html") "<span class=\"sentence\">"

sentenceEnd :: Inline
sentenceEnd = RawInline (Format "html") "</span>"

breakSentences :: Block -> Block
breakSentences (Para is) = Para $ sentenceStart : go False is
  where
    go _ [] = [sentenceEnd]
    go True (Space : iss) = sentenceEnd : (interSentenceSpace : (sentenceStart : go False iss))
    go True (SoftBreak : iss) = sentenceEnd : (interSentenceSpace : (sentenceStart : go False iss))
    go _ (i : iss) = i : go (isEndOfSentence i) iss

    isEndOfSentence (Str x) = lastChar == '.' || lastChar == '?' || lastChar == '!'
      where
        lastChar = T.last x
    isEndOfSentence _       = False
breakSentences x = x
