PROJECT = decirest
PROJECT_DESCRIPTION = Thin layer above cowboy for creating big REST apis
PROJECT_VERSION = 0.1.0

DEPS = cowboy jsx jesse erlydtl lager
dep_cowboy = git git@github.com:ninenines/cowboy.git 2.0.0-pre.8
SP = 2

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
