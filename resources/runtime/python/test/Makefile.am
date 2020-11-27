check-local: metrics test_api

metrics:
	if [ x"$$PYTHON°TEST" = x"yes" ]; then \
		./metrics.py -a rma.aadl -r rma.impl ; \
	fi

test_api:
	if [ x"$$PYTHON°TEST" = x"yes" ]; then \
			./test.py ; \
	fi
