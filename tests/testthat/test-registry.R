test_that("the declaration parser handles descriptors, kwargs expansion and systems", {
  dir <- tempfile("reg")
  dir.create(dir)
  writeLines(c(
    "from __future__ import annotations",
    "",
    "foo_descriptor = ModelDescriptor(display_name=\"Foo\", compute=\"gpu\")",
    "",
    "# a comment with a \"quote\"",
    "foo_method_metadata = foo_descriptor.method_metadata(",
    "    method=\"Foo-1\",  # inline comment",
    "    ag_key=\"FOO\",",
    "    cache_type=\"r2\",",
    "    suite=\"tabarena-2026-01-01\",",
    "    cache_kwargs={\"bucket\": \"tabarena\", \"prefix\": \"cache\"},",
    ")",
    "",
    "_sys_kwargs = dict(",
    "    method_class=\"system\",",
    "    suite=\"tabarena-2026-02-02\",",
    "    cache_type=\"r2\",",
    ")",
    "",
    "bar_metadata = MethodMetadata.system(",
    "    method=\"Bar (4h)\",",
    "    name=\"Bar 4h\",",
    "    **_sys_kwargs,",
    ")",
    "",
    "old_metadata = MethodMetadata.tabarena_legacy_s3(",
    "    method=\"Old\",",
    "    suite=\"tabarena-2025-06-12\",",
    ")",
    "",
    "foo_info = ModelInfo(model_cls=None, method_metadata=foo_method_metadata)"
  ), file.path(dir, "foo__info.py"))
  writeLines(c(
    "from tabarena.models.foo.info import foo_method_metadata as foo_alias",
    "from tabarena.systems.bar.info import (",
    "    bar_metadata,",
    ")",
    "",
    "tabarena_method_metadata_collection = MethodMetadataCollection(",
    "    method_metadata_lst=[",
    "        # Systems",
    "        bar_metadata,",
    "        foo_alias,",
    "    ],",
    ")"
  ), file.path(dir, "tabarena__methods.py"))
  reg <- ta_registry_refresh(save = FALSE, src_dir = dir)
  expect_equal(nrow(reg), 3)
  foo <- reg[reg$method == "Foo-1", ]
  expect_equal(foo$suite, "tabarena-2026-01-01")
  expect_equal(foo$cache_type, "r2")
  expect_equal(foo$method_type, "config")
  expect_equal(foo$method_class, "model")
  expect_true(foo$current)
  bar <- reg[reg$method == "Bar (4h)", ]
  expect_equal(bar$suite, "tabarena-2026-02-02")
  expect_equal(bar$method_class, "system")
  expect_equal(bar$method_type, "baseline")
  expect_equal(bar$display_name, "Bar 4h")
  expect_true(bar$current)
  old <- reg[reg$method == "Old", ]
  expect_equal(old$cache_type, "s3")
  expect_false(old$current)
})
