#!/usr/bin/env python
import subprocess
import re

tests_raw = subprocess.check_output(["ls", "tests"])
tests = re.sub("[^\w]", " ",  tests_raw).split()


OUTPUTS = {
    'program': "1",
    'test_if': "1",
    'test_if_false': "",
    'test_if_el': "1",
    'test_if_el_false': "0",
    'test_eq_int': "1",
    'test_eq_int_false': "0",
    'test_neq_int': "0",
    'test_neq_int_false': "1",
    'test_eq_bool_true_true': "1",
    'test_eq_bool_true_false': "0",
    'test_eq_bool_false_false': "1",
    'test_eq_bool_false_true': "0",
    'test_neq_bool_true_true': "0",
    'test_neq_bool_true_false': "1",
    'test_neq_bool_false_false': "0",
    'test_neq_bool_false_true': "1",
    'test_and_true_true': "1",
    'test_and_true_false': "0",
    'test_and_false_true': "0",
    'test_and_false_false': "0",
    'test_or_true_true': "1",
    'test_or_true_false': "1",
    'test_or_false_true': "1",
    'test_or_false_false': "0",
    'test_lt_true': "1",
    'test_lt_false': "0",
    'test_gt_true': "1",
    'test_gt_false': "0",
    'test_le_true': "1",
    'test_le_false': "0",
    'test_ge_true': "1",
    'test_ge_false': "0",
    'test_eq_char_false': "0",
    'test_eq_char_true': "1",
    'test_eq_string_true': "1",
    'test_print_string': "printed string",
    'test_print_char': 'c',
    'test_print_true': 'verum',
    'test_print_false': 'falsum',
    'test_le_string': 'falsum',
    'test_add_strings': 'first second',
    'test_python_str_cut_out': 'cut out',
    'test_python_str_cut_out_left': 'out',
    'test_python_str_cut_out_right': 'cut',
    'test_python_str_left_right': 'ut ou',
    'test_python_str_left_1': 'ut out',
    'test_python_str_right_6': 'cut ou',
    'test_string_elem': 'j',
    'test_string_elem_2': 'n',
    'test_string_length_0': '0',
    'test_string_length_6': '6',
    'test_char_ord': '99',
    'test_decl_int': '1',
    'test_decl_bool': 'verum',
    'test_decl_str': 'michal ma nowy strych',
    'test_decl_char': 'x',
    'test_eq_var_int': 'falsum',
    'test_change_string_content': 'mleko',
    'test_change_one_letter_string': 'jajka',
    'test_array_construct': '13',
    'test_sub_str_var': 't',
    'test_block_variables': 'titulus 2titulus 1',
    'test_stmt_decl': 'titulus',
    'test_x_plus_1': '11',
    'test_x_minus_1': '9',
    'test_while': '1112',
    'test_10_minus_1': '9',
    'test_for_loop': '12345678910',
    'test_bool_not': 'falsum',
    'test_array_write': '10\n20\n30\n40\n50\n60\n70\n80\n90\n100\n',
    'test_procedure': 'Maslo z orzechamiLubie placki',
    'test_global_variable': 'teraz ty!raz dwa trzy',
    'test_procedure_with_param': '1110',
    'test_procedure_with_two_params': 'ala ma kota',
    'test_procedure_with_variable_param': 'verum',
    'test_procedure_with_many_variables': '24',
    'test_procedure_recursion': '3628800',
    'test_function_return_0': '0',
    'test_function_recursive': '3628800',
    'test_function_recursive2': '3628800',
    'test_variables_strange': '109',
    'test_variables_strange': '109',
    'test_function_recursive_with_decls': 'falsumverum verum verum verum verum verum',
    'test_function_with_variable_params': '10000 10000',
    'test_array_function_param': 'verum falsum',
    'test_array_length': '10',
    'test_function_return_array': '1: 0\n2: 2\n3: 3\n4: 12\n5: 21\n6: 52\n7: 111\n8: 123\n9: 432\n10: 23423\n',
    'test_lege_int_simple': '123',
    'test_lege_minus_int': '-10',
    'test_lege_int_white_spaces': '80',
    'test_lege_int_white_spaces2': '-12',
    'test_lege_3_ints': '1251',
    'test_lege_char': 'c',
    'test_lege_10_char': 'abc\n10\nyhb',
    'test_lege_string': 'De vita Pascalis etiam pellicula perfecta est anno 1972, Roberto Rossellini moderatore.',
    'test_lege_3_strings': 'kochammojestudia',
    'test_dict_sample': '10',
    'test_dict_param': '1',
}

INPUTS = {
    'test_lege_int_simple': 'cat_123',
    'test_lege_minus_int': 'cat_minus_10',
    'test_lege_int_white_spaces': 'int_with_white_space',
    'test_lege_int_white_spaces2': 'white_spaces2',
    'test_lege_3_ints': '3_ints',
    'test_lege_char': 'c',
    'test_lege_10_char': '10_chars',
    'test_lege_string': 'string_input',
    'test_lege_3_strings': '3_strings',
}


def decorate_green(value):
    return '\033[1;32m{}\033[1;m'.format(value)


def decorate_red(value):
    return '\033[1;31m{}\033[1;m'.format(value)


def decorate_yellow(value):
    return '\033[1;33m{}\033[1;m'.format(value)


correct = 0
for numb, test in enumerate(tests, 1):
    try:
        if test in INPUTS:
            command = ["./TestPascalis -s tests/{} < inputs/{}".format(test, INPUTS[test])]
            output = subprocess.check_output(command, shell=True)
        else:
            output = subprocess.check_output(["./TestPascalis", "-s", "tests/{}".format(test)])
    except subprocess.CalledProcessError as exc:
        print '{}. program {} {}'.format(numb, test, decorate_red('cannot be excecuted'))
        print exc
        continue

    try:
        OUTPUTS[test]
    except KeyError:
        print "{}. Program {} results {}".format(numb, decorate_yellow(test), output)
        print "There is defaule value for this program"
        continue
    if output == OUTPUTS[test]:
        correct += 1
        print "{}. Test {} is {}.".format(numb, test, decorate_green("ok"))
    else:
        print "{}. Test {} is {}.".format(numb, test, decorate_red("failed"))
        print "output = {}, excepcted = {}".format(output, OUTPUTS[test])


test_count = len(tests)
missing_tests = set(OUTPUTS.keys()) - set(tests)
if missing_tests:
    for test in missing_tests:
        print 'missing test {}'.format(decorate_red(test))
else:
    if correct == test_count:
        print decorate_green("Everything is ok.")
        print decorate_green("{}/{} tests passed.".format(correct, correct))
    else:
        print decorate_red("{}/{} tests passed.".format(correct, test_count))
