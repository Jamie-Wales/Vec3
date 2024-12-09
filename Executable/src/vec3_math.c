#include "vec3_math.h"
#include "value.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

Vec3Value* vec3_add(Vec3Value** args)
{
    Vec3Value* a = args[0];
    Vec3Value* b = args[1];
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


Vec3Value* vec3_plot(Vec3Value** args)
{
    Vec3Value* x_vals = args[0];
    Vec3Value* y_vals = args[1];

    vec3_incref(x_vals);
    vec3_incref(y_vals);

    if (x_vals->object.type != TYPE_LIST || y_vals->object.type != TYPE_LIST) {
        vec3_decref(x_vals);
        vec3_decref(y_vals);
        return vec3_error(vec3_new_string("plot: both arguments must be lists"));
    }

    // Check lengths
    size_t length = x_vals->as.list->length;
    if (y_vals->as.list->length != length || length == 0) {
        vec3_decref(x_vals);
        vec3_decref(y_vals);
        return vec3_error(vec3_new_string("plot: lists must have the same non-zero length"));
    }

    double* x_data = malloc(length * sizeof(double));
    double* y_data = malloc(length * sizeof(double));

    vec3_list* x_node = x_vals->as.list;
    vec3_list* y_node = y_vals->as.list;

    // Extract numeric data and check that all elements are numbers
    for (size_t i = 0; i < length; i++) {
        if (x_node->value->object.type != TYPE_NUMBER ||
            y_node->value->object.type != TYPE_NUMBER) {
            free(x_data);
            free(y_data);
            vec3_decref(x_vals);
            vec3_decref(y_vals);
            return vec3_error(vec3_new_string("plot: both lists must contain only numbers"));
        }

        x_data[i] = number_to_double(&x_node->value->as.number);
        y_data[i] = number_to_double(&y_node->value->as.number);

        x_node = x_node->next;
        y_node = y_node->next;
    }

    FILE* gnuplotPipe = popen("gnuplot -persist", "w");
    if (gnuplotPipe == NULL) {
        free(x_data);
        free(y_data);
        vec3_decref(x_vals);
        vec3_decref(y_vals);
        return vec3_error(vec3_new_string("Failed to open gnuplot"));
    }

    fprintf(gnuplotPipe, "set title 'XY Plot'\n");
    fprintf(gnuplotPipe, "set xlabel 'X'\n");
    fprintf(gnuplotPipe, "set ylabel 'Y'\n");
    fprintf(gnuplotPipe, "plot '-' with lines title 'Data'\n");

    for (size_t i = 0; i < length; i++) {
        fprintf(gnuplotPipe, "%f %f\n", x_data[i], y_data[i]);
    }

    fprintf(gnuplotPipe, "e\n");
    fflush(gnuplotPipe);

    pclose(gnuplotPipe);

    free(x_data);
    free(y_data);

    vec3_decref(x_vals);
    vec3_decref(y_vals);

    return vec3_new_nil();
}
Vec3Value* vec3_multiply(Vec3Value** args)
{
    Vec3Value* a = args[0];
    Vec3Value* b = args[1];
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
    Vec3Value* base = args[0];
    Vec3Value* exp = args[1];
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
    Vec3Value* value = args[0];
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
    Vec3Value* value = args[0];
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
    Vec3Value* value = args[0];
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
    Vec3Value* value = args[0];
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
    Vec3Value* value = args[0];
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
    Vec3Value* value = args[0];
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
    Vec3Value* value = args[0];
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
    Vec3Value* value = args[0];
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
    Vec3Value* value = args[0];
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
    Vec3Value* value = args[0];
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
    Vec3Value* base = args[0];
    Vec3Value* value = args[1];
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
    Vec3Value* value = args[0];
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
    Vec3Value* a = args[0];
    Vec3Value* b = args[1];

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
    Vec3Value* a = args[0];
    Vec3Value* b = args[1];

    if (a->object.type != TYPE_LIST || b->object.type != TYPE_LIST || a->as.list->length != 3 || b->as.list->length != 3) {
        return vec3_new_nil();
    }

    return vec3_new_nil();
}
