export LANG='en_US.UTF-8'
export LC_ALL="en_US.UTF-8"
export MAVEN_OPTS="-Djava.awt.headless=true"

## Directory listing 
alias lh='ls -alhF'
alias ll='ls -alF'
alias lt='ls -altr'

## Development environment
alias mcc='mvn clean compile'
alias mct='mvn clean test'
alias mc='mvn compile'
alias mt='mvn test'
alias cot='cd $HOME/Projects/cotiviti'

alias gen-ensime='mvn dependency:sources dependency:resolve -Dclassifer=javadoc && mvn ensime:generate && mvn clean compile test-compile'
