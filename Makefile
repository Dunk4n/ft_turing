NAME		=	ft_turing
BUILD_DIR	=	_build

all:	$(NAME)

clean:
	@echo "  RM       $(BUILD_DIR)"
	@dune clean
	@rm -rf $(BUILD_DIR)

fclean: clean
	@echo "  RM       $(NAME)"
	@rm -f $(NAME)

$(NAME):
	@echo "  BUILD    $@"
	@dune build
	@cp _build/default/bin/main.exe ft_turing

re:
	@$(MAKE) fclean --no-print-directory
	@$(MAKE) all --no-print-directory

.PHONY: all clean fclean re
