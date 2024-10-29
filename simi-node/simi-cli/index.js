#!/usr/bin/env node
"use strict"
const { Command } = require('commander');
const testCommand = require('./bin/test.js');
const program = new Command();
program
    .name('simi')
    .description('A simple CLI application')
    .version('1.0.0')
program
    .command('echo <name>')
    .description('Echo messages')
    .option('-e, --excited', 'Make the greeting excited')
    .action((name, options) => {
        const greeting = options.excited ? `Hello, ${name}!!!` : `Hello, ${name}.`;
        console.log(greeting);
    });
program
    .command('test <message>')
    .description('Print test message')
    .action(testCommand);

program.parse(process.argv);
module.exports = program;
