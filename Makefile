
SFML = sfml-system sfml-graphics sfml-window

CXXFLAGS = -std=c++23 $(shell pkg-config --cflags $(SFML))
LDFLAGS = $(shell pkg-config --libs $(SFML))

SOURCES = $(wildcard src/*.cpp)
OBJDIR = obj
OBJECTS = $(patsubst src/%.cpp, $(OBJDIR)/%.o, $(SOURCES))
EXE = iso.exe

all: $(EXE) $(OBJDIR) compile_flags.txt

$(EXE): $(OBJECTS)
	$(CXX) $(OBJECTS) $(LDFLAGS) -o $@

obj/%.o: src/%.cpp
	$(CXX) $^ $(CXXFLAGS) -c -o $@

$(OBJDIR):
	mkdir -p $(OBJDIR)

compile_flags.txt:
	echo $(CXXFLAGS) $(LDFLAGS) | tr ' ' '\n' > compile_flags.txt

clean:
	rm $(OBJECTS) $(EXE) compile_flags.txt

.PHONY: all clean
