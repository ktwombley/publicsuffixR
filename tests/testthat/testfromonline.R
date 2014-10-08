# taken from http://mxr.mozilla.org/mozilla-central/source/netwerk/test/unit/data/test_psl.txt?raw=1
# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

library(publicsuffixR)
context("Sample Tests from website")

test_that("null input is handled", {
    
    # null input.
  expect_true(testthat:::compare(checkPublicSuffix(NA, NA), NA)$equal)
})

test_that("Mixed case.", {
  expect_true(checkPublicSuffix('COM', NA))
  expect_true(checkPublicSuffix('example.COM', 'example.com'))
  expect_true(checkPublicSuffix('WwW.example.COM', 'example.com'))
})
  
test_that("Leading dot.", {
  expect_true(checkPublicSuffix('.com', null))
  expect_true(checkPublicSuffix('.example', null))
  expect_true(checkPublicSuffix('.example.com', null))
  expect_true(checkPublicSuffix('.example.example', null))
})

test_that("unlisted TLD.", {
  expect_true(checkPublicSuffix('example', null))
  expect_true(checkPublicSuffix('example.example', 'example.example'))
  expect_true(checkPublicSuffix('b.example.example', 'example.example'))
  expect_true(checkPublicSuffix('a.b.example.example', 'example.example'))
})

test_that("Listed, but non-Internet, TLD.", {
  #expect_true(checkPublicSuffix('local', null))
  #expect_true(checkPublicSuffix('example.local', null))
  #expect_true(checkPublicSuffix('b.example.local', null))
  #expect_true(checkPublicSuffix('a.b.example.local', null))
})

test_that("TLD with only 1 rule.", {
  expect_true(checkPublicSuffix('biz', null))
  expect_true(checkPublicSuffix('domain.biz', 'domain.biz'))
  expect_true(checkPublicSuffix('b.domain.biz', 'domain.biz'))
  expect_true(checkPublicSuffix('a.b.domain.biz', 'domain.biz'))
})

test_that("TLD with some 2-level rules.", {
  expect_true(checkPublicSuffix('com', null))
  expect_true(checkPublicSuffix('example.com', 'example.com'))
  expect_true(checkPublicSuffix('b.example.com', 'example.com'))
  expect_true(checkPublicSuffix('a.b.example.com', 'example.com'))
  expect_true(checkPublicSuffix('uk.com', null))
  expect_true(checkPublicSuffix('example.uk.com', 'example.uk.com'))
  expect_true(checkPublicSuffix('b.example.uk.com', 'example.uk.com'))
  expect_true(checkPublicSuffix('a.b.example.uk.com', 'example.uk.com'))
  expect_true(checkPublicSuffix('test.ac', 'test.ac'))
})
  
test_that("TLD with only 1 (wildcard) rule.", {
  expect_true(checkPublicSuffix('cy', null))
  expect_true(checkPublicSuffix('c.cy', null))
  expect_true(checkPublicSuffix('b.c.cy', 'b.c.cy'))
  expect_true(checkPublicSuffix('a.b.c.cy', 'b.c.cy'))
})

test_that("More complex TLD.", {
  expect_true(checkPublicSuffix('jp', null))
  expect_true(checkPublicSuffix('test.jp', 'test.jp'))
  expect_true(checkPublicSuffix('www.test.jp', 'test.jp'))
  expect_true(checkPublicSuffix('ac.jp', null))
  expect_true(checkPublicSuffix('test.ac.jp', 'test.ac.jp'))
  expect_true(checkPublicSuffix('www.test.ac.jp', 'test.ac.jp'))
  expect_true(checkPublicSuffix('kyoto.jp', null))
  expect_true(checkPublicSuffix('test.kyoto.jp', 'test.kyoto.jp'))
  expect_true(checkPublicSuffix('ide.kyoto.jp', null))
  expect_true(checkPublicSuffix('b.ide.kyoto.jp', 'b.ide.kyoto.jp'))
  expect_true(checkPublicSuffix('a.b.ide.kyoto.jp', 'b.ide.kyoto.jp'))
  expect_true(checkPublicSuffix('c.kobe.jp', null))
  expect_true(checkPublicSuffix('b.c.kobe.jp', 'b.c.kobe.jp'))
  expect_true(checkPublicSuffix('a.b.c.kobe.jp', 'b.c.kobe.jp'))
  expect_true(checkPublicSuffix('city.kobe.jp', 'city.kobe.jp'))
  expect_true(checkPublicSuffix('www.city.kobe.jp', 'city.kobe.jp'))
})

test_that("TLD with a wildcard rule and exceptions.", {
  expect_true(checkPublicSuffix('ck', null))
  expect_true(checkPublicSuffix('test.ck', null))
  expect_true(checkPublicSuffix('b.test.ck', 'b.test.ck'))
  expect_true(checkPublicSuffix('a.b.test.ck', 'b.test.ck'))
  expect_true(checkPublicSuffix('www.ck', 'www.ck'))
  expect_true(checkPublicSuffix('www.www.ck', 'www.ck'))
})

test_that("US K12.", {
  expect_true(checkPublicSuffix('us', null))
  expect_true(checkPublicSuffix('test.us', 'test.us'))
  expect_true(checkPublicSuffix('www.test.us', 'test.us'))
  expect_true(checkPublicSuffix('ak.us', null))
  expect_true(checkPublicSuffix('test.ak.us', 'test.ak.us'))
  expect_true(checkPublicSuffix('www.test.ak.us', 'test.ak.us'))
  expect_true(checkPublicSuffix('k12.ak.us', null))
  expect_true(checkPublicSuffix('test.k12.ak.us', 'test.k12.ak.us'))
  expect_true(checkPublicSuffix('www.test.k12.ak.us', 'test.k12.ak.us'))
})

test_that("IDN labels.", {
  expect_true(checkPublicSuffix('食狮.com.cn', '食狮.com.cn'))
  expect_true(checkPublicSuffix('食狮.公司.cn', '食狮.公司.cn'))
  expect_true(checkPublicSuffix('www.食狮.公司.cn', '食狮.公司.cn'))
  expect_true(checkPublicSuffix('shishi.公司.cn', 'shishi.公司.cn'))
  expect_true(checkPublicSuffix('公司.cn', null))
  expect_true(checkPublicSuffix('食狮.中国', '食狮.中国'))
  expect_true(checkPublicSuffix('www.食狮.中国', '食狮.中国'))
  expect_true(checkPublicSuffix('shishi.中国', 'shishi.中国'))
  expect_true(checkPublicSuffix('中国', null))
})

test_that("Same as above, but punycoded.", {
  expect_true(checkPublicSuffix('xn--85x722f.com.cn', 'xn--85x722f.com.cn'))
  expect_true(checkPublicSuffix('xn--85x722f.xn--55qx5d.cn', 'xn--85x722f.xn--55qx5d.cn'))
  expect_true(checkPublicSuffix('www.xn--85x722f.xn--55qx5d.cn', 'xn--85x722f.xn--55qx5d.cn'))
  expect_true(checkPublicSuffix('shishi.xn--55qx5d.cn', 'shishi.xn--55qx5d.cn'))
  expect_true(checkPublicSuffix('xn--55qx5d.cn', null))
  expect_true(checkPublicSuffix('xn--85x722f.xn--fiqs8s', 'xn--85x722f.xn--fiqs8s'))
  expect_true(checkPublicSuffix('www.xn--85x722f.xn--fiqs8s', 'xn--85x722f.xn--fiqs8s'))
  expect_true(checkPublicSuffix('shishi.xn--fiqs8s', 'shishi.xn--fiqs8s'))
  expect_true(checkPublicSuffix('xn--fiqs8s', null))  
})