##
## EPITECH PROJECT, 2021
## image compressor
## File description:
## makefile
##

BINARY_PATH 	:=	$(shell stack path --local-install-root)
NAME 			= 	imageCompressor

all		:
			stack build
			cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

tests_run:
			stack test

clean	:
			stack clean

fclean	:	clean
			rm -f $(NAME)

re		:	fclean all

.PHONY	:	all clean fclean re