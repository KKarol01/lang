#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include "parser.hpp"

auto read() {
	std::ifstream file{"script.lang"};
	if(!file.is_open()) {return std::string{};}
	file.seekg(0, std::ios::end);
	const size_t size = file.tellg();
	std::string content(size, '\0');
	file.seekg(0);
	file.read(content.data(), content.size());
	return content;
}

void a() {
	auto text = read();
	std::stringstream ss{text};
	std::string read;
	while(ss >> read) {
		std::cout << read << '\n';
	}
}