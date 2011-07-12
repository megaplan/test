MODULES = client misc misc_rabbit server

all:
	for a in $(MODULES) ; do \
		make -C $$a ; \
	done
	
clean:
	for a in $(MODULES) ; do \
		make -C $$a clean ; \
	done

