module ParserSpec where

import Test.Hspec
import Parser

main :: IO ()
main = hspec $ do
    describe "absolute" $ do
    it "returns the original number when given a positive input" $
        parseFile "file.p4" `shouldBe` parsed


parsed :: IO [Statement]
parsed = [ParserHeader "ethernet_t" [ParserField ("bit<48>","dstAddr"),ParserField ("bit<48>","srcAddr"),ParserField ("bit<16>","etherType")],ParserHeader "ipv4_t" [ParserField ("bit<8>","ttl"),ParserField ("bit<16>","hdrChecksum"),ParserField ("bit<32>","srcAddr"),ParserField ("bit<32>","dstAddr")],ParserStruct "headers" [StructField ("ethernet_t","ethernet"),StructField ("ipv4_t","ipv4")],Parser [State "start" (ParserSeq [Transition "parse_ethernet"]),State "parse_ethernet" (ParserSeq [FuncExpr (Extract (FuncVar "packet") (FuncVar "hdr.ethernet")),Transition "parse_ipv4"]),State "parse_ipv4" (ParserSeq [FuncExpr (Extract (FuncVar "packet") (FuncVar "hdr.ipv4")),Transition "accept"])],Control "MyIngress" [ParserAction "mydrop" (ParserSeq [ParserDrop]),ParserAction "ipv4_ch" (ParserSeq [ParserAssignment "hdr.ethernet.srcAddr" (NumConstant "2"),ParserAssignment "hdr.ethernet.dstAddr" (NumConstant "1"),ParserAssignment "hdr.ipv4.ttl" (NumConstant "20"),FuncExpr (SetInvalid (FuncVar "hdr.ipv4"))]),ParserTable "ipv4_lpm" (Keys [Semi "hdr.ipv4.dstAddr" "lpm"]) (Acts ["ipv4_ch","mydrop","NoAction"]),Apply (ParserSeq [ParserIf (BoolExpr (IsValid (BoolVar "hdr.ipv4"))) (ParserSeq [FuncExpr (ApplyFunc (FuncVar "ipv4_lpm"))]) ParserSkip])],Control "MyDeparser" [Apply (ParserSeq [FuncExpr (Emit (FuncVar "packet") (FuncVar "hdr.ethernet")),FuncExpr (Emit (FuncVar "packet") (FuncVar "hdr.ipv4"))])]]
