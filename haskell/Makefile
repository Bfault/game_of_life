NAME		=		gameoflife

RPNAME		=		haskell

BINARY_PATH	:=		$(shell stack path --local-install-root)

all:
	stack build
	cp $(BINARY_PATH)/bin/$(RPNAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

re: fclean all