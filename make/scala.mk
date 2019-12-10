MAIN_FILE := main.scala

define run-scala
	java \
		-Dsbt.main.class="sbt.ScriptMain" \
		-Dsbt.boot.directory="$(HOME)/.sbt/boot" \
		-jar "/usr/share/sbt/bin/sbt-launch.jar" "$(1)"
endef
