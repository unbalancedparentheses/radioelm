PROJECT = uc

DEPS = cowboy epgsql lager jiffy ibrowse mixer sync

dep_cowboy = git https://github.com/ninenines/cowboy.git 1.0.1
dep_epgsql = git https://github.com/epgsql/epgsql.git 3.1.0
dep_lager = git https://github.com/basho/lager.git 3.0.1
dep_jiffy = git https://github.com/davisp/jiffy.git 0.14.2
dep_ibrowse = git https://github.com/cmullaparthi/ibrowse.git v4.1.2
dep_mixer = git https://github.com/opscode/mixer.git 0.1.1

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'

SHELL_OPTS = -name ${PROJECT}@`hostname` -s ${PROJECT} -config config/sys.config
