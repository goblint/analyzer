#!/usr/bin/env bash
sed -i 's/zstd : CPPFLAGS += -DZSTD_LEGACY_SUPPORT=$(ZSTD_LEGACY_SUPPORT).*/zstd : CPPFLAGS += -DZSTD_LEGACY_SUPPORT=$(ZSTD_LEGACY_SUPPORT) -DZSTD_NO_INTRINSICS/' programs/Makefile
compiledb make zstd
