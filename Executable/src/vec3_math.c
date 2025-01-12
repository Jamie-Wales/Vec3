#include "vec3_math.h"
#include "value.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char* plot_type_to_string(PlotType type)
{
    switch (type) {
    case PLOT_SCATTER:
        return "points";
    case PLOT_LINE:
        return "lines";
    case PLOT_BAR:
        return "boxes";
    case PLOT_SIGNAL:
        return "steps";
    default:
        return "points";
    }
}

PlotType parse_plot_type(const char* type_str)
{
    if (strcmp(type_str, "scatter") == 0)
        return PLOT_SCATTER;
    if (strcmp(type_str, "line") == 0)
        return PLOT_LINE;
    if (strcmp(type_str, "bar") == 0)
        return PLOT_BAR;
    if (strcmp(type_str, "signal") == 0)
        return PLOT_SIGNAL;
    return PLOT_SCATTER; // default
}

Vec3Value* get_config_value(Vec3Value* config, const char* key, Vec3Value* default_value)
{
    vec3_list* current = config->as.list;
    while (current != NULL) {
        if (current->value != NULL && current->value->object.type == TYPE_LIST && current->value->as.list != NULL && current->value->as.list->value != NULL && current->value->as.list->value->object.type == TYPE_STRING) {

            Vec3Value* key_value = current->value->as.list->value;
            if (strcmp(key_value->as.string.chars, key) == 0 && current->value->as.list->next != NULL) {
                return current->value->as.list->next->value;
            }
        }
        current = current->next;
    }
    return default_value;
}

Vec3Value* vec3_plot(Vec3Value** args)
{
    if (args[1]->object.type != TYPE_LIST) {
        return vec3_error(vec3_new_string("plot: expected configuration list"));
    }

    // Default values
    Vec3Value* default_title = vec3_new_string("Plot");
    Vec3Value* default_type = vec3_new_string("scatter");
    Vec3Value* default_empty_list = vec3_new_list(0);

    // Extract configuration
    Vec3Value* title = get_config_value(args[1], "title", default_title);
    Vec3Value* plot_type_val = get_config_value(args[1], "ptype", default_type);
    Vec3Value* x_vals = get_config_value(args[1], "x", default_empty_list);
    Vec3Value* y_vals = get_config_value(args[1], "y", default_empty_list);

    if (x_vals->object.type != TYPE_LIST || y_vals->object.type != TYPE_LIST) {
        vec3_decref(default_title);
        vec3_decref(default_type);
        vec3_decref(default_empty_list);
        return vec3_error(vec3_new_string("plot: x and y must be lists"));
    }

    size_t x_len = x_vals->as.list->length;
    size_t y_len = y_vals->as.list->length;

    if (x_len != y_len || x_len == 0) {
        vec3_decref(default_title);
        vec3_decref(default_type);
        vec3_decref(default_empty_list);
        return vec3_error(vec3_new_string("plot: x and y must have same non-zero length"));
    }

    // Extract data into arrays
    double* x_data = malloc(x_len * sizeof(double));
    double* y_data = malloc(y_len * sizeof(double));

    vec3_list* x_node = x_vals->as.list;
    vec3_list* y_node = y_vals->as.list;

    for (size_t i = 0; i < x_len; i++) {
        if (x_node->value->object.type != TYPE_NUMBER || y_node->value->object.type != TYPE_NUMBER) {
            free(x_data);
            free(y_data);
            vec3_decref(default_title);
            vec3_decref(default_type);
            vec3_decref(default_empty_list);
            return vec3_error(vec3_new_string("plot: data must be numeric"));
        }

        x_data[i] = number_to_double(&x_node->value->as.number);
        y_data[i] = number_to_double(&y_node->value->as.number);

        x_node = x_node->next;
        y_node = y_node->next;
    }

    // Open gnuplot pipe
    FILE* gnuplotPipe = popen("gnuplot -persist", "w");
    if (gnuplotPipe == NULL) {
        free(x_data);
        free(y_data);
        vec3_decref(default_title);
        vec3_decref(default_type);
        vec3_decref(default_empty_list);
        return vec3_error(vec3_new_string("Failed to open gnuplot"));
    }

    // Configure plot
    PlotType type = parse_plot_type(plot_type_val->as.string.chars);
    const char* plot_style = plot_type_to_string(type);

    fprintf(gnuplotPipe, "set title '%s'\n", title->as.string.chars);
    fprintf(gnuplotPipe, "set xlabel 'X'\n");
    fprintf(gnuplotPipe, "set ylabel 'Y'\n");
    fprintf(gnuplotPipe, "set grid\n");
    fprintf(gnuplotPipe, "set style fill solid 0.5\n");

    // Special handling for bar plots
    if (type == PLOT_BAR) {
        fprintf(gnuplotPipe, "set boxwidth 0.8\n");
        fprintf(gnuplotPipe, "set style fill solid\n");
    }

    // Plot data
    fprintf(gnuplotPipe, "plot '-' with %s title '%s'\n",
        plot_style, title->as.string.chars);

    for (size_t i = 0; i < x_len; i++) {
        fprintf(gnuplotPipe, "%f %f\n", x_data[i], y_data[i]);
    }

    fprintf(gnuplotPipe, "e\n");
    fflush(gnuplotPipe);
    pclose(gnuplotPipe);

    // Cleanup
    free(x_data);
    free(y_data);
    vec3_decref(default_title);
    vec3_decref(default_type);
    vec3_decref(default_empty_list);

    return vec3_new_nil();
}
Vec3Value* vec3_sub(Vec3Value** args)
{
    Vec3Value* a = args[1];
    Vec3Value* b = args[2];
    vec3_incref(a);
    vec3_incref(b);

    Vec3Value* result = NULL;

    if (a->object.type == TYPE_NUMBER && b->object.type == TYPE_NUMBER) {
        switch (a->as.number.type) {
        case NUMBER_INTEGER:
            if (b->as.number.type == NUMBER_INTEGER) {
                result = vec3_new_number(number_from_int(
                    a->as.number.as.integer - b->as.number.as.integer));
            }
            break;
        case NUMBER_FLOAT:
            if (b->as.number.type == NUMBER_FLOAT) {
                result = vec3_new_number(number_from_float(
                    a->as.number.as.float_val - b->as.number.as.float_val));
            }
            break;
        case NUMBER_RATIONAL:
            if (b->as.number.type == NUMBER_RATIONAL) {
                int64_t num = a->as.number.as.rational.num * b->as.number.as.rational.denom - b->as.number.as.rational.num * a->as.number.as.rational.denom;
                int64_t denom = a->as.number.as.rational.denom * b->as.number.as.rational.denom;
                result = vec3_new_number(number_from_rational(num, denom));
            }
            break;
        case NUMBER_COMPLEX:
            if (b->as.number.type == NUMBER_COMPLEX) {
                double real = a->as.number.as.complex.real - b->as.number.as.complex.real;
                double imag = a->as.number.as.complex.imag - b->as.number.as.complex.imag;
                result = vec3_new_number(number_from_complex(real, imag));
            }
            break;
        }
    }

    vec3_decref(a);
    vec3_decref(b);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_plot_function(Vec3Value** args)
{
    if (args[1]->object.type != TYPE_STRING || args[2]->object.type != TYPE_FUNCTION) {
        return vec3_error(vec3_new_string("plotFunc: expected (string, function) arguments"));
    }

    const char* title = args[1]->as.string.chars;
    Vec3Value* func = args[2];

    const int num_points = 1000;
    double* x_data = malloc(num_points * sizeof(double));
    double* y_data = malloc(num_points * sizeof(double));
    if (!x_data || !y_data) {
        free(x_data);
        free(y_data);
        return vec3_error(vec3_new_string("Failed to allocate memory for plot data"));
    }

    double start = -10.0;
    double end = 10.0;
    double step = (end - start) / (num_points - 1);

    for (int i = 0; i < num_points; i++) {
        x_data[i] = start + i * step;

        Vec3Value* x_val = vec3_new_number(number_from_float(x_data[i]));
        Vec3Value* fn_args[] = { x_val };
        Vec3Value* y_val = vec3_call_function(func, fn_args, 1);

        if (y_val->object.type != TYPE_NUMBER) {
            free(x_data);
            free(y_data);
            vec3_decref(x_val);
            vec3_decref(y_val);
            return vec3_error(vec3_new_string("Function must return a number"));
        }

        y_data[i] = number_to_double(&y_val->as.number);
        vec3_decref(x_val);
        vec3_decref(y_val);
    }

    FILE* gnuplotPipe = popen("gnuplot -persist", "w");
    if (gnuplotPipe == NULL) {
        free(x_data);
        free(y_data);
        return vec3_error(vec3_new_string("Failed to open gnuplot"));
    }

    fprintf(gnuplotPipe, "set title '%s'\n", title);
    fprintf(gnuplotPipe, "set xlabel 'X'\n");
    fprintf(gnuplotPipe, "set ylabel 'Y'\n");
    fprintf(gnuplotPipe, "set grid\n");
    fprintf(gnuplotPipe, "plot '-' with lines title '%s'\n", title);

    for (int i = 0; i < num_points; i++) {
        fprintf(gnuplotPipe, "%f %f\n", x_data[i], y_data[i]);
    }

    fprintf(gnuplotPipe, "e\n");
    fflush(gnuplotPipe);
    pclose(gnuplotPipe);
    free(x_data);
    free(y_data);
    return vec3_new_nil();
}

Vec3Value* vec3_plot_functions(Vec3Value** args)
{
    if (args[1]->object.type != TYPE_STRING || args[2]->object.type != TYPE_LIST) {
        return vec3_error(vec3_new_string("plotFuncs: expected (string, [functions]) arguments"));
    }

    vec3_list* funcs = args[2]->as.list;

    // Verify all elements are functions
    vec3_list* current = funcs;
    while (current != NULL) {
        if (current->value->object.type != TYPE_FUNCTION) {
            return vec3_error(vec3_new_string("All elements in list must be functions"));
        }
        current = current->next;
    }

    const char* title = args[1]->as.string.chars;
    const int num_points = 1000;
    const int num_funcs = args[2]->as.list->length;

    FILE* gnuplotPipe = popen("gnuplot -persist", "w");
    if (gnuplotPipe == NULL) {
        return vec3_error(vec3_new_string("Failed to open gnuplot"));
    }

    fprintf(gnuplotPipe, "set title '%s'\n", title);
    fprintf(gnuplotPipe, "set xlabel 'X'\n");
    fprintf(gnuplotPipe, "set ylabel 'Y'\n");
    fprintf(gnuplotPipe, "set grid\n");

    fprintf(gnuplotPipe, "plot ");
    for (int f = 0; f < num_funcs; f++) {
        if (f > 0)
            fprintf(gnuplotPipe, ", ");
        fprintf(gnuplotPipe, "'-' with lines title 'f%d'", f + 1);
    }
    fprintf(gnuplotPipe, "\n");

    while (funcs != NULL) {
        Vec3Value* func = funcs->value;
        double* x_data = malloc(num_points * sizeof(double));
        double* y_data = malloc(num_points * sizeof(double));
        if (!x_data || !y_data) {
            free(x_data);
            free(y_data);
            pclose(gnuplotPipe);
            return vec3_error(vec3_new_string("Failed to allocate memory"));
        }

        double start = -10.0;
        double end = 10.0;
        double step = (end - start) / (num_points - 1);

        for (int i = 0; i < num_points; i++) {
            x_data[i] = start + i * step;
            Vec3Value* x_val = vec3_new_number(number_from_float(x_data[i]));
            Vec3Value* fn_args[] = { x_val };
            Vec3Value* y_val = vec3_call_function(func, fn_args, 1);

            if (y_val->object.type != TYPE_NUMBER) {
                free(x_data);
                free(y_data);
                vec3_decref(x_val);
                vec3_decref(y_val);
                pclose(gnuplotPipe);
                return vec3_error(vec3_new_string("Function must return a number"));
            }

            y_data[i] = number_to_double(&y_val->as.number);
            vec3_decref(x_val);
            vec3_decref(y_val);
        }

        for (int i = 0; i < num_points; i++) {
            fprintf(gnuplotPipe, "%f %f\n", x_data[i], y_data[i]);
        }
        fprintf(gnuplotPipe, "e\n");

        free(x_data);
        free(y_data);
        funcs = funcs->next;
    }

    fflush(gnuplotPipe);
    pclose(gnuplotPipe);
    return vec3_new_nil();
}
Vec3Value* vec3_add(Vec3Value** args)
{
    Vec3Value* a = args[1];
    Vec3Value* b = args[2];
    vec3_incref(a);
    vec3_incref(b);

    Vec3Value* result = NULL;

    if (a->object.type == TYPE_NUMBER && b->object.type == TYPE_NUMBER) {
        Number result_num;
        switch (a->as.number.type) {
        case NUMBER_INTEGER:
            if (b->as.number.type == NUMBER_INTEGER) {
                result_num = number_from_int(
                    a->as.number.as.integer + b->as.number.as.integer);
                result = vec3_new_number(result_num);
            }
            break;
        case NUMBER_FLOAT:
            if (b->as.number.type == NUMBER_FLOAT) {
                result_num = number_from_float(
                    a->as.number.as.float_val + b->as.number.as.float_val);
                result = vec3_new_number(result_num);
            }
            break;
        case NUMBER_RATIONAL:
            if (b->as.number.type == NUMBER_RATIONAL) {
                int64_t num = a->as.number.as.rational.num * b->as.number.as.rational.denom + b->as.number.as.rational.num * a->as.number.as.rational.denom;
                int64_t denom = a->as.number.as.rational.denom * b->as.number.as.rational.denom;
                result = vec3_new_number(number_from_rational(num, denom));
            }
            break;
        case NUMBER_COMPLEX:
            if (b->as.number.type == NUMBER_COMPLEX) {
                double real = a->as.number.as.complex.real + b->as.number.as.complex.real;
                double imag = a->as.number.as.complex.imag + b->as.number.as.complex.imag;
                result = vec3_new_number(number_from_complex(real, imag));
            }
            break;
        }
    }

    vec3_decref(a);
    vec3_decref(b);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_multiply(Vec3Value** args)
{
    Vec3Value* a = args[1];
    Vec3Value* b = args[2];
    vec3_incref(a);
    vec3_incref(b);

    Vec3Value* result = NULL;

    if (a->object.type == TYPE_NUMBER && b->object.type == TYPE_NUMBER) {
        switch (a->as.number.type) {
        case NUMBER_INTEGER:
            if (b->as.number.type == NUMBER_INTEGER) {
                result = vec3_new_number(number_from_int(
                    a->as.number.as.integer * b->as.number.as.integer));
            }
            break;
        case NUMBER_FLOAT:
            if (b->as.number.type == NUMBER_FLOAT) {
                result = vec3_new_number(number_from_float(
                    a->as.number.as.float_val * b->as.number.as.float_val));
            }
            break;
        case NUMBER_RATIONAL:
            if (b->as.number.type == NUMBER_RATIONAL) {
                result = vec3_new_number(number_from_rational(
                    a->as.number.as.rational.num * b->as.number.as.rational.num,
                    a->as.number.as.rational.denom * b->as.number.as.rational.denom));
            }
            break;
        case NUMBER_COMPLEX:
            if (b->as.number.type == NUMBER_COMPLEX) {
                double ar = a->as.number.as.complex.real;
                double ai = a->as.number.as.complex.imag;
                double br = b->as.number.as.complex.real;
                double bi = b->as.number.as.complex.imag;
                result = vec3_new_number(number_from_complex(
                    ar * br - ai * bi,
                    ar * bi + ai * br));
            }
            break;
        }
    }

    vec3_decref(a);
    vec3_decref(b);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_power(Vec3Value** args)
{
    Vec3Value* base = args[1];
    Vec3Value* exp = args[2];
    vec3_incref(base);
    vec3_incref(exp);
    Vec3Value* result = NULL;

    if (base->object.type == TYPE_NUMBER && exp->object.type == TYPE_NUMBER) {
        double base_val = 0.0, exp_val = 0.0;

        switch (base->as.number.type) {
        case NUMBER_INTEGER:
            base_val = (double)base->as.number.as.integer;
            break;
        case NUMBER_FLOAT:
            base_val = base->as.number.as.float_val;
            break;
        case NUMBER_RATIONAL:
            base_val = (double)base->as.number.as.rational.num / base->as.number.as.rational.denom;
            break;
        case NUMBER_COMPLEX:
            base_val = base->as.number.as.complex.real;
            break;
        }

        switch (exp->as.number.type) {
        case NUMBER_INTEGER:
            exp_val = (double)exp->as.number.as.integer;
            break;
        case NUMBER_FLOAT:
            exp_val = exp->as.number.as.float_val;
            break;
        case NUMBER_RATIONAL:
            exp_val = (double)exp->as.number.as.rational.num / exp->as.number.as.rational.denom;
            break;
        case NUMBER_COMPLEX:
            exp_val = exp->as.number.as.complex.real;
            break;
        }

        result = vec3_new_number(number_from_float(pow(base_val, exp_val)));
    }

    vec3_decref(base);
    vec3_decref(exp);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_sqrt(Vec3Value** args)
{
    Vec3Value* value = args[1];
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        double val = 0.0;
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            val = sqrt((double)value->as.number.as.integer);
            break;
        case NUMBER_FLOAT:
            val = sqrt(value->as.number.as.float_val);
            break;
        case NUMBER_RATIONAL:
            val = sqrt((double)value->as.number.as.rational.num / value->as.number.as.rational.denom);
            break;
        case NUMBER_COMPLEX:
            fprintf(stderr, "Complex square root not implemented\n");
            break;
        }
        result = vec3_new_number(number_from_float(val));
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_abs(Vec3Value** args)
{
    Vec3Value* value = args[1];
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            result = vec3_new_number(number_from_int(
                llabs(value->as.number.as.integer)));
            break;
        case NUMBER_FLOAT:
            result = vec3_new_number(number_from_float(
                fabs(value->as.number.as.float_val)));
            break;
        case NUMBER_RATIONAL:
            result = vec3_new_number(number_from_rational(
                llabs(value->as.number.as.rational.num),
                llabs(value->as.number.as.rational.denom)));
            break;
        case NUMBER_COMPLEX: {
            double real = value->as.number.as.complex.real;
            double imag = value->as.number.as.complex.imag;
            result = vec3_new_number(number_from_float(
                sqrt(real * real + imag * imag)));
            break;
        }
        }
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_floor(Vec3Value** args)
{
    Vec3Value* value = args[1];
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            result = vec3_new_number(value->as.number); // Already integral
            break;
        case NUMBER_FLOAT:
            result = vec3_new_number(number_from_float(
                floor(value->as.number.as.float_val)));
            break;
        case NUMBER_RATIONAL:
            result = vec3_new_number(number_from_int(
                value->as.number.as.rational.num / value->as.number.as.rational.denom));
            break;
        case NUMBER_COMPLEX:
            fprintf(stderr, "Cannot floor complex numbers\n");
            break;
        }
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_cos(Vec3Value** args)
{
    Vec3Value* value = args[1];
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        double val = 0.0;
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            val = cos((double)value->as.number.as.integer);
            break;
        case NUMBER_FLOAT:
            val = cos(value->as.number.as.float_val);
            break;
        case NUMBER_RATIONAL:
            val = cos((double)value->as.number.as.rational.num / value->as.number.as.rational.denom);
            break;
        case NUMBER_COMPLEX:
            fprintf(stderr, "Complex cosine not implemented\n");
            break;
        }
        result = vec3_new_number(number_from_float(val));
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_sin(Vec3Value** args)
{
    Vec3Value* value = args[1];
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        double val = 0.0;
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            val = sin((double)value->as.number.as.integer);
            break;
        case NUMBER_FLOAT:
            val = sin(value->as.number.as.float_val);
            break;
        case NUMBER_RATIONAL:
            val = sin((double)value->as.number.as.rational.num / value->as.number.as.rational.denom);
            break;
        case NUMBER_COMPLEX:
            fprintf(stderr, "Complex sine not implemented\n");
            break;
        }
        result = vec3_new_number(number_from_float(val));
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_tan(Vec3Value** args)
{
    Vec3Value* value = args[1];
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        double val = 0.0;
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            val = tan((double)value->as.number.as.integer);
            break;
        case NUMBER_FLOAT:
            val = tan(value->as.number.as.float_val);
            break;
        case NUMBER_RATIONAL:
            val = tan((double)value->as.number.as.rational.num / value->as.number.as.rational.denom);
            break;
        case NUMBER_COMPLEX:
            fprintf(stderr, "Complex tangent not implemented\n");
            break;
        }
        result = vec3_new_number(number_from_float(val));
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_acos(Vec3Value** args)
{
    Vec3Value* value = args[1];
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        double val = 0.0;
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            val = acos((double)value->as.number.as.integer);
            break;
        case NUMBER_FLOAT:
            val = acos(value->as.number.as.float_val);
            break;
        case NUMBER_RATIONAL:
            val = acos((double)value->as.number.as.rational.num / value->as.number.as.rational.denom);
            break;
        case NUMBER_COMPLEX:
            fprintf(stderr, "Complex arc cosine not implemented\n");
            break;
        }
        result = vec3_new_number(number_from_float(val));
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_asin(Vec3Value** args)
{
    Vec3Value* value = args[1];
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        double val = 0.0;
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            val = asin((double)value->as.number.as.integer);
            break;
        case NUMBER_FLOAT:
            val = asin(value->as.number.as.float_val);
            break;
        case NUMBER_RATIONAL:
            val = asin((double)value->as.number.as.rational.num / value->as.number.as.rational.denom);
            break;
        case NUMBER_COMPLEX:
            fprintf(stderr, "Complex arc sine not implemented\n");
            break;
        }
        result = vec3_new_number(number_from_float(val));
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}
Vec3Value* vec3_atan(Vec3Value** args)
{
    Vec3Value* value = args[1];
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        double val = 0.0;
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            val = atan((double)value->as.number.as.integer);
            break;
        case NUMBER_FLOAT:
            val = atan(value->as.number.as.float_val);
            break;
        case NUMBER_RATIONAL:
            val = atan((double)value->as.number.as.rational.num / value->as.number.as.rational.denom);
            break;
        case NUMBER_COMPLEX:
            fprintf(stderr, "Complex arc tangent not implemented\n");
            break;
        }
        result = vec3_new_number(number_from_float(val));
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_exp(Vec3Value** args)
{
    Vec3Value* value = args[1];
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        double val = 0.0;
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            val = exp((double)value->as.number.as.integer);
            break;
        case NUMBER_FLOAT:
            val = exp(value->as.number.as.float_val);
            break;
        case NUMBER_RATIONAL:
            val = exp((double)value->as.number.as.rational.num / value->as.number.as.rational.denom);
            break;
        case NUMBER_COMPLEX:
            fprintf(stderr, "Complex exponential not implemented\n");
            break;
        }
        result = vec3_new_number(number_from_float(val));
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_log(Vec3Value** args)
{
    Vec3Value* base = args[1];
    Vec3Value* value = args[2];
    vec3_incref(base);
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (base->object.type == TYPE_NUMBER && value->object.type == TYPE_NUMBER) {
        double base_val = 0.0, val = 0.0;

        // Get base value
        switch (base->as.number.type) {
        case NUMBER_INTEGER:
            base_val = (double)base->as.number.as.integer;
            break;
        case NUMBER_FLOAT:
            base_val = base->as.number.as.float_val;
            break;
        case NUMBER_RATIONAL:
            base_val = (double)base->as.number.as.rational.num / base->as.number.as.rational.denom;
            break;
        case NUMBER_COMPLEX:
            fprintf(stderr, "Complex logarithm base not supported\n");
            break;
        }

        // Get value
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            val = (double)value->as.number.as.integer;
            break;
        case NUMBER_FLOAT:
            val = value->as.number.as.float_val;
            break;
        case NUMBER_RATIONAL:
            val = (double)value->as.number.as.rational.num / value->as.number.as.rational.denom;
            break;
        case NUMBER_COMPLEX:
            fprintf(stderr, "Complex logarithm not implemented\n");
            break;
        }

        if (base_val > 0 && val > 0) {
            result = vec3_new_number(number_from_float(log(val) / log(base_val)));
        }
    }

    vec3_decref(base);
    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_log10(Vec3Value** args)
{
    Vec3Value* value = args[1];
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        double val = 0.0;
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            val = log10((double)value->as.number.as.integer);
            break;
        case NUMBER_FLOAT:
            val = log10(value->as.number.as.float_val);
            break;
        case NUMBER_RATIONAL:
            val = log10((double)value->as.number.as.rational.num / value->as.number.as.rational.denom);
            break;
        case NUMBER_COMPLEX:
            fprintf(stderr, "Complex log10 not implemented\n");
            break;
        }
        result = vec3_new_number(number_from_float(val));
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_dot_product(Vec3Value** args)
{
    Vec3Value* a = args[1];
    Vec3Value* b = args[2];

    // Check if both arguments are lists
    if (a->object.type != TYPE_LIST || b->object.type != TYPE_LIST) {
        return vec3_new_nil();
    }

    // Get the vectors
    vec3_list* vec_a = a->as.list;
    vec3_list* vec_b = b->as.list;

    // Check that vectors have same length
    if (vec_a->length != vec_b->length) {
        return vec3_new_nil();
    }

    double result = 0.0;
    vec3_list* curr_a = vec_a;
    vec3_list* curr_b = vec_b;

    while (curr_a != NULL && curr_b != NULL) {
        if (curr_a->value->object.type == TYPE_NUMBER && curr_b->value->object.type == TYPE_NUMBER) {
            double a_val = 0.0, b_val = 0.0;

            // Get values from a
            switch (curr_a->value->as.number.type) {
            case NUMBER_INTEGER:
                a_val = (double)curr_a->value->as.number.as.integer;
                break;
            case NUMBER_FLOAT:
                a_val = curr_a->value->as.number.as.float_val;
                break;
            case NUMBER_RATIONAL:
                a_val = (double)curr_a->value->as.number.as.rational.num / curr_a->value->as.number.as.rational.denom;
                break;
            default:
                return vec3_new_nil();
            }

            // Get values from b
            switch (curr_b->value->as.number.type) {
            case NUMBER_INTEGER:
                b_val = (double)curr_b->value->as.number.as.integer;
                break;
            case NUMBER_FLOAT:
                b_val = curr_b->value->as.number.as.float_val;
                break;
            case NUMBER_RATIONAL:
                b_val = (double)curr_b->value->as.number.as.rational.num / curr_b->value->as.number.as.rational.denom;
                break;
            default:
                return vec3_new_nil();
            }

            result += a_val * b_val;
        }

        curr_a = curr_a->next;
        curr_b = curr_b->next;
    }

    return vec3_new_number(number_from_float(result));
}

Vec3Value* vec3_cross_product(Vec3Value** args)
{
    Vec3Value* a = args[1];
    Vec3Value* b = args[2];

    if (a->object.type != TYPE_LIST || b->object.type != TYPE_LIST || a->as.list->length != 3 || b->as.list->length != 3) {
        return vec3_new_nil();
    }

    return vec3_new_nil();
}
