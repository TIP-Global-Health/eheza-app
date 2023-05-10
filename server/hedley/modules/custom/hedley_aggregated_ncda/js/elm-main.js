(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $ccapndave$elm_update_extra$Update$Extra$andThen = F3(
	function (update, msg, _v0) {
		var model = _v0.a;
		var cmd = _v0.b;
		var _v1 = A2(update, msg, model);
		var model_ = _v1.a;
		var cmd_ = _v1.b;
		return _Utils_Tuple2(
			model_,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[cmd, cmd_])));
	});
var $ccapndave$elm_update_extra$Update$Extra$sequence = F3(
	function (update, msgs, init) {
		var foldUpdate = $ccapndave$elm_update_extra$Update$Extra$andThen(update);
		return A3($elm$core$List$foldl, foldUpdate, init, msgs);
	});
var $Gizra$elm_fetch$Update$Fetch$applyFetch = F3(
	function (fetch, update, resultSoFar) {
		var msgs = fetch(resultSoFar.a);
		return $elm$core$List$isEmpty(msgs) ? resultSoFar : A3(
			$Gizra$elm_fetch$Update$Fetch$applyFetch,
			fetch,
			update,
			A3($ccapndave$elm_update_extra$Update$Extra$sequence, update, msgs, resultSoFar));
	});
var $Gizra$elm_fetch$Update$Fetch$andThenFetch = F4(
	function (fetch, update, msg, model) {
		return A3(
			$Gizra$elm_fetch$Update$Fetch$applyFetch,
			fetch,
			update,
			A2(update, msg, model));
	});
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$App$Model$MsgBackend = function (a) {
	return {$: 'MsgBackend', a: a};
};
var $author$project$Pages$Scoreboard$Fetch$fetch = F2(
	function (modelBackend, model) {
		return _List_Nil;
	});
var $author$project$App$Fetch$fetch = function (model) {
	var _v0 = model.activePage;
	switch (_v0.$) {
		case 'Menu':
			return _List_Nil;
		case 'Scoreboard':
			return A2(
				$elm$core$List$map,
				function (subMsg) {
					return $author$project$App$Model$MsgBackend(subMsg);
				},
				A2($author$project$Pages$Scoreboard$Fetch$fetch, model.backend, model.scoreboardPage));
		default:
			return _List_Nil;
	}
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $author$project$Backend$Model$MsgScoreboard = function (a) {
	return {$: 'MsgScoreboard', a: a};
};
var $author$project$App$Model$SetCurrentTime = function (a) {
	return {$: 'SetCurrentTime', a: a};
};
var $author$project$Backend$Scoreboard$Model$SetData = function (a) {
	return {$: 'SetData', a: a};
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $author$project$App$Types$English = {$: 'English'};
var $author$project$App$Types$NotFound = {$: 'NotFound'};
var $author$project$Pages$Menu$Model$emptyModel = {cell: $elm$core$Maybe$Nothing, district: $elm$core$Maybe$Nothing, province: $elm$core$Maybe$Nothing, sector: $elm$core$Maybe$Nothing, village: $elm$core$Maybe$Nothing};
var $author$project$Pages$Scoreboard$Model$emptyModel = {yearSelectorGap: 0};
var $author$project$Backend$Model$emptyModelBackend = {scoreboardData: $elm$core$Maybe$Nothing};
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $author$project$App$Model$emptyModel = {
	activePage: $author$project$App$Types$NotFound,
	backend: $author$project$Backend$Model$emptyModelBackend,
	currentTime: $elm$time$Time$millisToPosix(0),
	errors: _List_Nil,
	language: $author$project$App$Types$English,
	menuPage: $author$project$Pages$Menu$Model$emptyModel,
	scoreboardPage: $author$project$Pages$Scoreboard$Model$emptyModel
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $author$project$App$Types$Menu = {$: 'Menu'};
var $author$project$App$Types$Scoreboard = {$: 'Scoreboard'};
var $author$project$App$Update$resolveActivePage = function (page) {
	switch (page) {
		case 'menu':
			return $author$project$App$Types$Menu;
		case 'results':
			return $author$project$App$Types$Scoreboard;
		default:
			return $author$project$App$Types$NotFound;
	}
};
var $author$project$App$Model$MsgMenuPage = function (a) {
	return {$: 'MsgMenuPage', a: a};
};
var $author$project$App$Model$MsgScoreboardPage = function (a) {
	return {$: 'MsgScoreboardPage', a: a};
};
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$App$Model$PagesReturn = F4(
	function (model, cmd, error, appMsgs) {
		return {appMsgs: appMsgs, cmd: cmd, error: error, model: model};
	});
var $author$project$Error$Utils$noError = $elm$core$Maybe$Nothing;
var $author$project$Pages$Menu$Update$update = F2(
	function (msg, model) {
		var updatedFunc = msg.a;
		var value = msg.b;
		return A4(
			$author$project$App$Model$PagesReturn,
			A2(updatedFunc, value, model),
			$elm$core$Platform$Cmd$none,
			$author$project$Error$Utils$noError,
			_List_Nil);
	});
var $author$project$Pages$Scoreboard$Update$update = F3(
	function (modelBackend, msg, model) {
		var step = msg.a;
		return A4(
			$author$project$App$Model$PagesReturn,
			_Utils_update(
				model,
				{yearSelectorGap: model.yearSelectorGap + step}),
			$elm$core$Platform$Cmd$none,
			$author$project$Error$Utils$noError,
			_List_Nil);
	});
var $author$project$Backend$Types$BackendReturn = F4(
	function (model, cmd, error, appMsgs) {
		return {appMsgs: appMsgs, cmd: cmd, error: error, model: model};
	});
var $author$project$Backend$Scoreboard$Model$ScoreboardData = F3(
	function (entityName, entityType, records) {
		return {entityName: entityName, entityType: entityType, records: records};
	});
var $author$project$Backend$Scoreboard$Model$PatientData = function (birthDate) {
	return {birthDate: birthDate};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 'Bad', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 'Good', a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parseA(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					var _v2 = callback(a);
					var parseB = _v2.a;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
					}
				}
			});
	});
var $elm$parser$Parser$andThen = $elm$parser$Parser$Advanced$andThen;
var $elm$parser$Parser$UnexpectedChar = {$: 'UnexpectedChar'};
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 'AddRight', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {col: col, contextStack: contextStack, problem: problem, row: row};
	});
var $elm$parser$Parser$Advanced$Empty = {$: 'Empty'};
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.row, s.col, x, s.context));
	});
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return $elm$parser$Parser$Advanced$Parser(
			function (s) {
				var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, s.offset, s.src);
				return _Utils_eq(newOffset, -1) ? A2(
					$elm$parser$Parser$Advanced$Bad,
					false,
					A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
					$elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: 1, context: s.context, indent: s.indent, offset: s.offset + 1, row: s.row + 1, src: s.src}) : A3(
					$elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: s.col + 1, context: s.context, indent: s.indent, offset: newOffset, row: s.row, src: s.src}));
			});
	});
var $elm$parser$Parser$chompIf = function (isGood) {
	return A2($elm$parser$Parser$Advanced$chompIf, isGood, $elm$parser$Parser$UnexpectedChar);
};
var $justinmimbs$date$Date$deadEndToString = function (_v0) {
	var problem = _v0.problem;
	if (problem.$ === 'Problem') {
		var message = problem.a;
		return message;
	} else {
		return 'Expected a date in ISO 8601 format';
	}
};
var $elm$parser$Parser$ExpectingEnd = {$: 'ExpectingEnd'};
var $elm$parser$Parser$Advanced$end = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return _Utils_eq(
				$elm$core$String$length(s.src),
				s.offset) ? A3($elm$parser$Parser$Advanced$Good, false, _Utils_Tuple0, s) : A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $elm$parser$Parser$end = $elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd);
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v2 = parseA(s0);
				if (_v2.$ === 'Bad') {
					var p = _v2.a;
					var x = _v2.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v2.a;
					var a = _v2.b;
					var s1 = _v2.c;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3(
							$elm$parser$Parser$Advanced$Good,
							p1 || p2,
							A2(func, a, b),
							s2);
					}
				}
			});
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						func(a),
						s1);
				} else {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				}
			});
	});
var $elm$parser$Parser$map = $elm$parser$Parser$Advanced$map;
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 'Append', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
		});
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $justinmimbs$date$Date$MonthAndDay = F2(
	function (a, b) {
		return {$: 'MonthAndDay', a: a, b: b};
	});
var $justinmimbs$date$Date$OrdinalDay = function (a) {
	return {$: 'OrdinalDay', a: a};
};
var $justinmimbs$date$Date$WeekAndWeekday = F2(
	function (a, b) {
		return {$: 'WeekAndWeekday', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$backtrackable = function (_v0) {
	var parse = _v0.a;
	return $elm$parser$Parser$Advanced$Parser(
		function (s0) {
			var _v1 = parse(s0);
			if (_v1.$ === 'Bad') {
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, false, x);
			} else {
				var a = _v1.b;
				var s1 = _v1.c;
				return A3($elm$parser$Parser$Advanced$Good, false, a, s1);
			}
		});
};
var $elm$parser$Parser$backtrackable = $elm$parser$Parser$Advanced$backtrackable;
var $elm$parser$Parser$Advanced$commit = function (a) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$Good, true, a, s);
		});
};
var $elm$parser$Parser$commit = $elm$parser$Parser$Advanced$commit;
var $elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						A2(
							func,
							A3($elm$core$String$slice, s0.offset, s1.offset, s0.src),
							a),
						s1);
				}
			});
	});
var $elm$parser$Parser$mapChompedString = $elm$parser$Parser$Advanced$mapChompedString;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $justinmimbs$date$Date$int1 = A2(
	$elm$parser$Parser$mapChompedString,
	F2(
		function (str, _v0) {
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(str));
		}),
	$elm$parser$Parser$chompIf($elm$core$Char$isDigit));
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$Good, false, a, s);
		});
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $justinmimbs$date$Date$int2 = A2(
	$elm$parser$Parser$mapChompedString,
	F2(
		function (str, _v0) {
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(str));
		}),
	A2(
		$elm$parser$Parser$ignorer,
		A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed(_Utils_Tuple0),
			$elm$parser$Parser$chompIf($elm$core$Char$isDigit)),
		$elm$parser$Parser$chompIf($elm$core$Char$isDigit)));
var $justinmimbs$date$Date$int3 = A2(
	$elm$parser$Parser$mapChompedString,
	F2(
		function (str, _v0) {
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(str));
		}),
	A2(
		$elm$parser$Parser$ignorer,
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed(_Utils_Tuple0),
				$elm$parser$Parser$chompIf($elm$core$Char$isDigit)),
			$elm$parser$Parser$chompIf($elm$core$Char$isDigit)),
		$elm$parser$Parser$chompIf($elm$core$Char$isDigit)));
var $elm$parser$Parser$Expecting = function (a) {
	return {$: 'Expecting', a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 'Token', a: a, b: b};
	});
var $elm$parser$Parser$toToken = function (str) {
	return A2(
		$elm$parser$Parser$Advanced$Token,
		str,
		$elm$parser$Parser$Expecting(str));
};
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$core$Basics$not = _Basics_not;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _v1.a;
			var newRow = _v1.b;
			var newCol = _v1.c;
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				$elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var $elm$parser$Parser$token = function (str) {
	return $elm$parser$Parser$Advanced$token(
		$elm$parser$Parser$toToken(str));
};
var $justinmimbs$date$Date$dayOfYear = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed($elm$core$Basics$identity),
				$elm$parser$Parser$token('-')),
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						$elm$parser$Parser$backtrackable(
						A2(
							$elm$parser$Parser$andThen,
							$elm$parser$Parser$commit,
							A2($elm$parser$Parser$map, $justinmimbs$date$Date$OrdinalDay, $justinmimbs$date$Date$int3))),
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							$elm$parser$Parser$succeed($justinmimbs$date$Date$MonthAndDay),
							$justinmimbs$date$Date$int2),
						$elm$parser$Parser$oneOf(
							_List_fromArray(
								[
									A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed($elm$core$Basics$identity),
										$elm$parser$Parser$token('-')),
									$justinmimbs$date$Date$int2),
									$elm$parser$Parser$succeed(1)
								]))),
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$succeed($justinmimbs$date$Date$WeekAndWeekday),
								$elm$parser$Parser$token('W')),
							$justinmimbs$date$Date$int2),
						$elm$parser$Parser$oneOf(
							_List_fromArray(
								[
									A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed($elm$core$Basics$identity),
										$elm$parser$Parser$token('-')),
									$justinmimbs$date$Date$int1),
									$elm$parser$Parser$succeed(1)
								])))
					]))),
			$elm$parser$Parser$backtrackable(
			A2(
				$elm$parser$Parser$andThen,
				$elm$parser$Parser$commit,
				A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						$elm$parser$Parser$succeed($justinmimbs$date$Date$MonthAndDay),
						$justinmimbs$date$Date$int2),
					$elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								$justinmimbs$date$Date$int2,
								$elm$parser$Parser$succeed(1)
							]))))),
			A2($elm$parser$Parser$map, $justinmimbs$date$Date$OrdinalDay, $justinmimbs$date$Date$int3),
			A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed($justinmimbs$date$Date$WeekAndWeekday),
					$elm$parser$Parser$token('W')),
				$justinmimbs$date$Date$int2),
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						$justinmimbs$date$Date$int1,
						$elm$parser$Parser$succeed(1)
					]))),
			$elm$parser$Parser$succeed(
			$justinmimbs$date$Date$OrdinalDay(1))
		]));
var $justinmimbs$date$Date$RD = function (a) {
	return {$: 'RD', a: a};
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm$core$Basics$neq = _Utils_notEqual;
var $justinmimbs$date$Date$isLeapYear = function (y) {
	return ((!A2($elm$core$Basics$modBy, 4, y)) && (!(!A2($elm$core$Basics$modBy, 100, y)))) || (!A2($elm$core$Basics$modBy, 400, y));
};
var $justinmimbs$date$Date$daysBeforeMonth = F2(
	function (y, m) {
		var leapDays = $justinmimbs$date$Date$isLeapYear(y) ? 1 : 0;
		switch (m.$) {
			case 'Jan':
				return 0;
			case 'Feb':
				return 31;
			case 'Mar':
				return 59 + leapDays;
			case 'Apr':
				return 90 + leapDays;
			case 'May':
				return 120 + leapDays;
			case 'Jun':
				return 151 + leapDays;
			case 'Jul':
				return 181 + leapDays;
			case 'Aug':
				return 212 + leapDays;
			case 'Sep':
				return 243 + leapDays;
			case 'Oct':
				return 273 + leapDays;
			case 'Nov':
				return 304 + leapDays;
			default:
				return 334 + leapDays;
		}
	});
var $justinmimbs$date$Date$floorDiv = F2(
	function (a, b) {
		return $elm$core$Basics$floor(a / b);
	});
var $justinmimbs$date$Date$daysBeforeYear = function (y1) {
	var y = y1 - 1;
	var leapYears = (A2($justinmimbs$date$Date$floorDiv, y, 4) - A2($justinmimbs$date$Date$floorDiv, y, 100)) + A2($justinmimbs$date$Date$floorDiv, y, 400);
	return (365 * y) + leapYears;
};
var $justinmimbs$date$Date$daysInMonth = F2(
	function (y, m) {
		switch (m.$) {
			case 'Jan':
				return 31;
			case 'Feb':
				return $justinmimbs$date$Date$isLeapYear(y) ? 29 : 28;
			case 'Mar':
				return 31;
			case 'Apr':
				return 30;
			case 'May':
				return 31;
			case 'Jun':
				return 30;
			case 'Jul':
				return 31;
			case 'Aug':
				return 31;
			case 'Sep':
				return 30;
			case 'Oct':
				return 31;
			case 'Nov':
				return 30;
			default:
				return 31;
		}
	});
var $justinmimbs$date$Date$isBetweenInt = F3(
	function (a, b, x) {
		return (_Utils_cmp(a, x) < 1) && (_Utils_cmp(x, b) < 1);
	});
var $justinmimbs$date$Date$monthToName = function (m) {
	switch (m.$) {
		case 'Jan':
			return 'January';
		case 'Feb':
			return 'February';
		case 'Mar':
			return 'March';
		case 'Apr':
			return 'April';
		case 'May':
			return 'May';
		case 'Jun':
			return 'June';
		case 'Jul':
			return 'July';
		case 'Aug':
			return 'August';
		case 'Sep':
			return 'September';
		case 'Oct':
			return 'October';
		case 'Nov':
			return 'November';
		default:
			return 'December';
	}
};
var $elm$time$Time$Apr = {$: 'Apr'};
var $elm$time$Time$Aug = {$: 'Aug'};
var $elm$time$Time$Dec = {$: 'Dec'};
var $elm$time$Time$Feb = {$: 'Feb'};
var $elm$time$Time$Jan = {$: 'Jan'};
var $elm$time$Time$Jul = {$: 'Jul'};
var $elm$time$Time$Jun = {$: 'Jun'};
var $elm$time$Time$Mar = {$: 'Mar'};
var $elm$time$Time$May = {$: 'May'};
var $elm$time$Time$Nov = {$: 'Nov'};
var $elm$time$Time$Oct = {$: 'Oct'};
var $elm$time$Time$Sep = {$: 'Sep'};
var $justinmimbs$date$Date$numberToMonth = function (mn) {
	var _v0 = A2($elm$core$Basics$max, 1, mn);
	switch (_v0) {
		case 1:
			return $elm$time$Time$Jan;
		case 2:
			return $elm$time$Time$Feb;
		case 3:
			return $elm$time$Time$Mar;
		case 4:
			return $elm$time$Time$Apr;
		case 5:
			return $elm$time$Time$May;
		case 6:
			return $elm$time$Time$Jun;
		case 7:
			return $elm$time$Time$Jul;
		case 8:
			return $elm$time$Time$Aug;
		case 9:
			return $elm$time$Time$Sep;
		case 10:
			return $elm$time$Time$Oct;
		case 11:
			return $elm$time$Time$Nov;
		default:
			return $elm$time$Time$Dec;
	}
};
var $justinmimbs$date$Date$fromCalendarParts = F3(
	function (y, mn, d) {
		return (!A3($justinmimbs$date$Date$isBetweenInt, 1, 12, mn)) ? $elm$core$Result$Err(
			'Invalid date: ' + (('month ' + ($elm$core$String$fromInt(mn) + ' is out of range')) + (' (1 to 12)' + ('; received (year ' + ($elm$core$String$fromInt(y) + (', month ' + ($elm$core$String$fromInt(mn) + (', day ' + ($elm$core$String$fromInt(d) + ')'))))))))) : ((!A3(
			$justinmimbs$date$Date$isBetweenInt,
			1,
			A2(
				$justinmimbs$date$Date$daysInMonth,
				y,
				$justinmimbs$date$Date$numberToMonth(mn)),
			d)) ? $elm$core$Result$Err(
			'Invalid date: ' + (('day ' + ($elm$core$String$fromInt(d) + ' is out of range')) + ((' (1 to ' + ($elm$core$String$fromInt(
				A2(
					$justinmimbs$date$Date$daysInMonth,
					y,
					$justinmimbs$date$Date$numberToMonth(mn))) + ')')) + ((' for ' + $justinmimbs$date$Date$monthToName(
				$justinmimbs$date$Date$numberToMonth(mn))) + ((((mn === 2) && (d === 29)) ? (' (' + ($elm$core$String$fromInt(y) + ' is not a leap year)')) : '') + ('; received (year ' + ($elm$core$String$fromInt(y) + (', month ' + ($elm$core$String$fromInt(mn) + (', day ' + ($elm$core$String$fromInt(d) + ')'))))))))))) : $elm$core$Result$Ok(
			$justinmimbs$date$Date$RD(
				($justinmimbs$date$Date$daysBeforeYear(y) + A2(
					$justinmimbs$date$Date$daysBeforeMonth,
					y,
					$justinmimbs$date$Date$numberToMonth(mn))) + d)));
	});
var $justinmimbs$date$Date$fromOrdinalParts = F2(
	function (y, od) {
		var daysInYear = $justinmimbs$date$Date$isLeapYear(y) ? 366 : 365;
		return (!A3($justinmimbs$date$Date$isBetweenInt, 1, daysInYear, od)) ? $elm$core$Result$Err(
			'Invalid ordinal date: ' + (('ordinal-day ' + ($elm$core$String$fromInt(od) + ' is out of range')) + ((' (1 to ' + ($elm$core$String$fromInt(daysInYear) + ')')) + ((' for ' + $elm$core$String$fromInt(y)) + ('; received (year ' + ($elm$core$String$fromInt(y) + (', ordinal-day ' + ($elm$core$String$fromInt(od) + ')')))))))) : $elm$core$Result$Ok(
			$justinmimbs$date$Date$RD(
				$justinmimbs$date$Date$daysBeforeYear(y) + od));
	});
var $justinmimbs$date$Date$weekdayNumber = function (_v0) {
	var rd = _v0.a;
	var _v1 = A2($elm$core$Basics$modBy, 7, rd);
	if (!_v1) {
		return 7;
	} else {
		var n = _v1;
		return n;
	}
};
var $justinmimbs$date$Date$daysBeforeWeekYear = function (y) {
	var jan4 = $justinmimbs$date$Date$daysBeforeYear(y) + 4;
	return jan4 - $justinmimbs$date$Date$weekdayNumber(
		$justinmimbs$date$Date$RD(jan4));
};
var $justinmimbs$date$Date$firstOfYear = function (y) {
	return $justinmimbs$date$Date$RD(
		$justinmimbs$date$Date$daysBeforeYear(y) + 1);
};
var $justinmimbs$date$Date$is53WeekYear = function (y) {
	var wdnJan1 = $justinmimbs$date$Date$weekdayNumber(
		$justinmimbs$date$Date$firstOfYear(y));
	return (wdnJan1 === 4) || ((wdnJan1 === 3) && $justinmimbs$date$Date$isLeapYear(y));
};
var $justinmimbs$date$Date$fromWeekParts = F3(
	function (wy, wn, wdn) {
		var weeksInYear = $justinmimbs$date$Date$is53WeekYear(wy) ? 53 : 52;
		return (!A3($justinmimbs$date$Date$isBetweenInt, 1, weeksInYear, wn)) ? $elm$core$Result$Err(
			'Invalid week date: ' + (('week ' + ($elm$core$String$fromInt(wn) + ' is out of range')) + ((' (1 to ' + ($elm$core$String$fromInt(weeksInYear) + ')')) + ((' for ' + $elm$core$String$fromInt(wy)) + ('; received (year ' + ($elm$core$String$fromInt(wy) + (', week ' + ($elm$core$String$fromInt(wn) + (', weekday ' + ($elm$core$String$fromInt(wdn) + ')')))))))))) : ((!A3($justinmimbs$date$Date$isBetweenInt, 1, 7, wdn)) ? $elm$core$Result$Err(
			'Invalid week date: ' + (('weekday ' + ($elm$core$String$fromInt(wdn) + ' is out of range')) + (' (1 to 7)' + ('; received (year ' + ($elm$core$String$fromInt(wy) + (', week ' + ($elm$core$String$fromInt(wn) + (', weekday ' + ($elm$core$String$fromInt(wdn) + ')'))))))))) : $elm$core$Result$Ok(
			$justinmimbs$date$Date$RD(
				($justinmimbs$date$Date$daysBeforeWeekYear(wy) + ((wn - 1) * 7)) + wdn)));
	});
var $justinmimbs$date$Date$fromYearAndDayOfYear = function (_v0) {
	var y = _v0.a;
	var doy = _v0.b;
	switch (doy.$) {
		case 'MonthAndDay':
			var mn = doy.a;
			var d = doy.b;
			return A3($justinmimbs$date$Date$fromCalendarParts, y, mn, d);
		case 'WeekAndWeekday':
			var wn = doy.a;
			var wdn = doy.b;
			return A3($justinmimbs$date$Date$fromWeekParts, y, wn, wdn);
		default:
			var od = doy.a;
			return A2($justinmimbs$date$Date$fromOrdinalParts, y, od);
	}
};
var $justinmimbs$date$Date$int4 = A2(
	$elm$parser$Parser$mapChompedString,
	F2(
		function (str, _v0) {
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(str));
		}),
	A2(
		$elm$parser$Parser$ignorer,
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed(_Utils_Tuple0),
						$elm$parser$Parser$oneOf(
							_List_fromArray(
								[
									$elm$parser$Parser$chompIf(
									function (c) {
										return _Utils_eq(
											c,
											_Utils_chr('-'));
									}),
									$elm$parser$Parser$succeed(_Utils_Tuple0)
								]))),
					$elm$parser$Parser$chompIf($elm$core$Char$isDigit)),
				$elm$parser$Parser$chompIf($elm$core$Char$isDigit)),
			$elm$parser$Parser$chompIf($elm$core$Char$isDigit)),
		$elm$parser$Parser$chompIf($elm$core$Char$isDigit)));
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$parser$Parser$Problem = function (a) {
	return {$: 'Problem', a: a};
};
var $elm$parser$Parser$Advanced$problem = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $elm$parser$Parser$problem = function (msg) {
	return $elm$parser$Parser$Advanced$problem(
		$elm$parser$Parser$Problem(msg));
};
var $justinmimbs$date$Date$resultToParser = function (result) {
	if (result.$ === 'Ok') {
		var x = result.a;
		return $elm$parser$Parser$succeed(x);
	} else {
		var message = result.a;
		return $elm$parser$Parser$problem(message);
	}
};
var $justinmimbs$date$Date$parser = A2(
	$elm$parser$Parser$andThen,
	A2($elm$core$Basics$composeR, $justinmimbs$date$Date$fromYearAndDayOfYear, $justinmimbs$date$Date$resultToParser),
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($elm$core$Tuple$pair),
			$justinmimbs$date$Date$int4),
		$justinmimbs$date$Date$dayOfYear));
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {col: col, problem: problem, row: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.row, p.col, p.problem);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 'Empty':
					return list;
				case 'AddRight':
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0.a;
		var _v1 = parse(
			{col: 1, context: _List_Nil, indent: 1, offset: 0, row: 1, src: src});
		if (_v1.$ === 'Good') {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (_v0.$ === 'Ok') {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $justinmimbs$date$Date$fromIsoString = A2(
	$elm$core$Basics$composeR,
	$elm$parser$Parser$run(
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($elm$core$Basics$identity),
			A2(
				$elm$parser$Parser$ignorer,
				$justinmimbs$date$Date$parser,
				A2(
					$elm$parser$Parser$andThen,
					$justinmimbs$date$Date$resultToParser,
					$elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								A2($elm$parser$Parser$map, $elm$core$Result$Ok, $elm$parser$Parser$end),
								A2(
								$elm$parser$Parser$map,
								$elm$core$Basics$always(
									$elm$core$Result$Err('Expected a date only, not a date and time')),
								$elm$parser$Parser$chompIf(
									$elm$core$Basics$eq(
										_Utils_chr('T')))),
								$elm$parser$Parser$succeed(
								$elm$core$Result$Err('Expected a date only'))
							])))))),
	$elm$core$Result$mapError(
		A2(
			$elm$core$Basics$composeR,
			$elm$core$List$head,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Maybe$map($justinmimbs$date$Date$deadEndToString),
				$elm$core$Maybe$withDefault('')))));
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm_community$json_extra$Json$Decode$Extra$fromResult = function (result) {
	if (result.$ === 'Ok') {
		var successValue = result.a;
		return $elm$json$Json$Decode$succeed(successValue);
	} else {
		var errorMessage = result.a;
		return $elm$json$Json$Decode$fail(errorMessage);
	}
};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Gizra$NominalDate$decodeYYYYMMDD = A2(
	$elm$json$Json$Decode$andThen,
	A2($elm$core$Basics$composeL, $elm_community$json_extra$Json$Decode$Extra$fromResult, $justinmimbs$date$Date$fromIsoString),
	$elm$json$Json$Decode$string);
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A2($elm$json$Json$Decode$field, key, valDecoder),
			decoder);
	});
var $author$project$Backend$Scoreboard$Decoder$decodePatientData = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'birth_date',
	$author$project$Gizra$NominalDate$decodeYYYYMMDD,
	$elm$json$Json$Decode$succeed($author$project$Backend$Scoreboard$Model$PatientData));
var $author$project$Backend$Scoreboard$Model$EntityCell = {$: 'EntityCell'};
var $author$project$Backend$Scoreboard$Model$EntityDistrict = {$: 'EntityDistrict'};
var $author$project$Backend$Scoreboard$Model$EntitySector = {$: 'EntitySector'};
var $author$project$Backend$Scoreboard$Model$EntityVillage = {$: 'EntityVillage'};
var $author$project$Backend$Scoreboard$Decoder$decodeSelectedEntity = A2(
	$elm$json$Json$Decode$andThen,
	function (entityType) {
		switch (entityType) {
			case 'district':
				return $elm$json$Json$Decode$succeed($author$project$Backend$Scoreboard$Model$EntityDistrict);
			case 'sector':
				return $elm$json$Json$Decode$succeed($author$project$Backend$Scoreboard$Model$EntitySector);
			case 'cell':
				return $elm$json$Json$Decode$succeed($author$project$Backend$Scoreboard$Model$EntityCell);
			case 'village':
				return $elm$json$Json$Decode$succeed($author$project$Backend$Scoreboard$Model$EntityVillage);
			default:
				return $elm$json$Json$Decode$fail(entityType + ' is unknown SelectedEntity type');
		}
	},
	$elm$json$Json$Decode$string);
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Backend$Scoreboard$Decoder$decodeScoreboardData = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'results',
	$elm$json$Json$Decode$list($author$project$Backend$Scoreboard$Decoder$decodePatientData),
	A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'entity_type',
		$author$project$Backend$Scoreboard$Decoder$decodeSelectedEntity,
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'entity_name',
			$elm$json$Json$Decode$string,
			$elm$json$Json$Decode$succeed($author$project$Backend$Scoreboard$Model$ScoreboardData))));
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$Backend$Scoreboard$Update$update = F2(
	function (msg, model) {
		var value = msg.a;
		var modelUpdated = _Utils_update(
			model,
			{
				scoreboardData: $elm$core$Maybe$Just(
					A2($elm$json$Json$Decode$decodeValue, $author$project$Backend$Scoreboard$Decoder$decodeScoreboardData, value))
			});
		return A4($author$project$Backend$Types$BackendReturn, modelUpdated, $elm$core$Platform$Cmd$none, $author$project$Error$Utils$noError, _List_Nil);
	});
var $elm$core$Platform$Cmd$map = _Platform_map;
var $author$project$Backend$Utils$updateSubModel = F4(
	function (subMsg, updateFunc, msg, model) {
		var backendReturn = A2(updateFunc, subMsg, model);
		return {
			appMsgs: backendReturn.appMsgs,
			cmd: A2($elm$core$Platform$Cmd$map, msg, backendReturn.cmd),
			error: backendReturn.error,
			model: backendReturn.model
		};
	});
var $author$project$Backend$Update$updateBackend = F2(
	function (msg, model) {
		var subMsg = msg.a;
		return A4(
			$author$project$Backend$Utils$updateSubModel,
			subMsg,
			F2(
				function (subMsg_, model_) {
					return A2($author$project$Backend$Scoreboard$Update$update, subMsg_, model_);
				}),
			function (subCmds) {
				return $author$project$Backend$Model$MsgScoreboard(subCmds);
			},
			model);
	});
var $elm_community$maybe_extra$Maybe$Extra$unwrap = F3(
	function (d, f, m) {
		if (m.$ === 'Nothing') {
			return d;
		} else {
			var a = m.a;
			return f(a);
		}
	});
var $author$project$App$Utils$handleErrors = F2(
	function (maybeError, model) {
		var errors = A3(
			$elm_community$maybe_extra$Maybe$Extra$unwrap,
			model.errors,
			function (error) {
				return A2($elm$core$List$cons, error, model.errors);
			},
			maybeError);
		return _Utils_update(
			model,
			{errors: errors});
	});
var $author$project$App$Utils$updateSubModel = F6(
	function (subMsg, subModel, updateFunc, modelUpdateFunc, msg, model) {
		var pagesReturn = A2(updateFunc, subMsg, subModel);
		var modelUpdatedWithError = A2($author$project$App$Utils$handleErrors, pagesReturn.error, model);
		var appCmds = $elm$core$List$isEmpty(pagesReturn.appMsgs) ? $elm$core$Platform$Cmd$none : $elm$core$Platform$Cmd$batch(
			A2(
				$elm$core$List$map,
				function (msg_) {
					return A2(
						$elm$core$Task$perform,
						$elm$core$Basics$identity,
						$elm$core$Task$succeed(msg_));
				},
				pagesReturn.appMsgs));
		return _Utils_Tuple2(
			A2(modelUpdateFunc, pagesReturn.model, modelUpdatedWithError),
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						A2($elm$core$Platform$Cmd$map, msg, pagesReturn.cmd),
						appCmds
					])));
	});
var $author$project$App$Update$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'MsgBackend':
				var subMsg = msg.a;
				return A6(
					$author$project$App$Utils$updateSubModel,
					subMsg,
					model.backend,
					F2(
						function (subMsg_, subModel) {
							return A2($author$project$Backend$Update$updateBackend, subMsg_, subModel);
						}),
					F2(
						function (subModel, model_) {
							return _Utils_update(
								model_,
								{backend: subModel});
						}),
					function (subCmds) {
						return $author$project$App$Model$MsgBackend(subCmds);
					},
					model);
			case 'MsgMenuPage':
				var subMsg = msg.a;
				return A6(
					$author$project$App$Utils$updateSubModel,
					subMsg,
					model.menuPage,
					F2(
						function (subMsg_, subModel) {
							return A2($author$project$Pages$Menu$Update$update, subMsg_, subModel);
						}),
					F2(
						function (subModel, model_) {
							return _Utils_update(
								model_,
								{menuPage: subModel});
						}),
					function (subCmds) {
						return $author$project$App$Model$MsgMenuPage(subCmds);
					},
					model);
			case 'MsgScoreboardPage':
				var subMsg = msg.a;
				return A6(
					$author$project$App$Utils$updateSubModel,
					subMsg,
					model.scoreboardPage,
					F2(
						function (subMsg_, subModel) {
							return A3($author$project$Pages$Scoreboard$Update$update, model.backend, subMsg_, subModel);
						}),
					F2(
						function (subModel, model_) {
							return _Utils_update(
								model_,
								{scoreboardPage: subModel});
						}),
					function (subCmds) {
						return $author$project$App$Model$MsgScoreboardPage(subCmds);
					},
					model);
			case 'SetActivePage':
				var activePage = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{activePage: activePage}),
					$elm$core$Platform$Cmd$none);
			default:
				var date = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{currentTime: date}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$App$Update$init = function (flags) {
	var activePage = $author$project$App$Update$resolveActivePage(flags.page);
	var model = _Utils_update(
		$author$project$App$Model$emptyModel,
		{activePage: activePage});
	var modelWithAppData = function () {
		var _v0 = model.activePage;
		switch (_v0.$) {
			case 'Menu':
				return model;
			case 'Scoreboard':
				return A2(
					$author$project$App$Update$update,
					$author$project$App$Model$MsgBackend(
						$author$project$Backend$Model$MsgScoreboard(
							$author$project$Backend$Scoreboard$Model$SetData(flags.appData))),
					model).a;
			default:
				return model;
		}
	}();
	var cmds = $elm$core$Platform$Cmd$batch(
		A2(
			$elm$core$List$append,
			_List_fromArray(
				[
					A2($elm$core$Task$perform, $author$project$App$Model$SetCurrentTime, $elm$time$Time$now)
				]),
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeR,
					$elm$core$Task$succeed,
					$elm$core$Task$perform($elm$core$Basics$identity)),
				$author$project$App$Fetch$fetch(modelWithAppData))));
	return _Utils_Tuple2(modelWithAppData, cmds);
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$App$Update$subscriptions = function (model) {
	return $elm$core$Platform$Sub$none;
};
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $justinmimbs$date$Date$fromCalendarDate = F3(
	function (y, m, d) {
		return $justinmimbs$date$Date$RD(
			($justinmimbs$date$Date$daysBeforeYear(y) + A2($justinmimbs$date$Date$daysBeforeMonth, y, m)) + A3(
				$elm$core$Basics$clamp,
				1,
				A2($justinmimbs$date$Date$daysInMonth, y, m),
				d));
	});
var $elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return $elm$core$Basics$floor(numerator / denominator);
	});
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.start, posixMinutes) < 0) {
					return posixMinutes + era.offset;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var $elm$time$Time$toAdjustedMinutes = F2(
	function (_v0, time) {
		var defaultOffset = _v0.a;
		var eras = _v0.b;
		return A3(
			$elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var $elm$core$Basics$ge = _Utils_ge;
var $elm$time$Time$toCivil = function (minutes) {
	var rawDay = A2($elm$time$Time$flooredDiv, minutes, 60 * 24) + 719468;
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		day: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		month: month,
		year: year + ((month <= 2) ? 1 : 0)
	};
};
var $elm$time$Time$toDay = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).day;
	});
var $elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _v0 = $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).month;
		switch (_v0) {
			case 1:
				return $elm$time$Time$Jan;
			case 2:
				return $elm$time$Time$Feb;
			case 3:
				return $elm$time$Time$Mar;
			case 4:
				return $elm$time$Time$Apr;
			case 5:
				return $elm$time$Time$May;
			case 6:
				return $elm$time$Time$Jun;
			case 7:
				return $elm$time$Time$Jul;
			case 8:
				return $elm$time$Time$Aug;
			case 9:
				return $elm$time$Time$Sep;
			case 10:
				return $elm$time$Time$Oct;
			case 11:
				return $elm$time$Time$Nov;
			default:
				return $elm$time$Time$Dec;
		}
	});
var $elm$time$Time$toYear = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).year;
	});
var $justinmimbs$date$Date$fromPosix = F2(
	function (zone, posix) {
		return A3(
			$justinmimbs$date$Date$fromCalendarDate,
			A2($elm$time$Time$toYear, zone, posix),
			A2($elm$time$Time$toMonth, zone, posix),
			A2($elm$time$Time$toDay, zone, posix));
	});
var $elm$time$Time$utc = A2($elm$time$Time$Zone, 0, _List_Nil);
var $author$project$Gizra$NominalDate$fromLocalDateTime = $justinmimbs$date$Date$fromPosix($elm$time$Time$utc);
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $author$project$Utils$Html$emptyNode = $elm$html$Html$text('');
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $elm$html$Html$li = _VirtualDom_node('li');
var $author$project$Translate$ErrorBadPayload = function (a) {
	return {$: 'ErrorBadPayload', a: a};
};
var $author$project$Translate$ErrorBadStatus = function (a) {
	return {$: 'ErrorBadStatus', a: a};
};
var $author$project$Translate$ErrorBadUrl = {$: 'ErrorBadUrl'};
var $author$project$Translate$ErrorNetworkError = {$: 'ErrorNetworkError'};
var $author$project$Translate$ErrorTimeout = {$: 'ErrorTimeout'};
var $author$project$Translate$HttpError = function (a) {
	return {$: 'HttpError', a: a};
};
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $author$project$Translate$Cell = {$: 'Cell'};
var $author$project$Translate$District = {$: 'District'};
var $author$project$Translate$Sector = {$: 'Sector'};
var $author$project$Translate$Village = {$: 'Village'};
var $author$project$Translate$translateHttpError = function (transId) {
	switch (transId.$) {
		case 'ErrorBadUrl':
			return {english: 'URL is not valid.', kinyarwanda: $elm$core$Maybe$Nothing};
		case 'ErrorBadPayload':
			var message = transId.a;
			return {english: 'The server responded with data of an unexpected type: ' + message, kinyarwanda: $elm$core$Maybe$Nothing};
		case 'ErrorBadStatus':
			var err = transId.a;
			return {english: err, kinyarwanda: $elm$core$Maybe$Nothing};
		case 'ErrorNetworkError':
			return {english: 'There was a network error.', kinyarwanda: $elm$core$Maybe$Nothing};
		default:
			return {english: 'The network request timed out.', kinyarwanda: $elm$core$Maybe$Nothing};
	}
};
var $author$project$Translate$translateMonth = F2(
	function (month, _short) {
		switch (month.$) {
			case 'Jan':
				return _short ? {
					english: 'Jan',
					kinyarwanda: $elm$core$Maybe$Just('Mut')
				} : {
					english: 'January',
					kinyarwanda: $elm$core$Maybe$Just('Mutarama')
				};
			case 'Feb':
				return _short ? {
					english: 'Feb',
					kinyarwanda: $elm$core$Maybe$Just('Gas')
				} : {
					english: 'February',
					kinyarwanda: $elm$core$Maybe$Just('Gashyantare')
				};
			case 'Mar':
				return _short ? {
					english: 'Mar',
					kinyarwanda: $elm$core$Maybe$Just('Wer')
				} : {
					english: 'March',
					kinyarwanda: $elm$core$Maybe$Just('Werurwe')
				};
			case 'Apr':
				return _short ? {
					english: 'Apr',
					kinyarwanda: $elm$core$Maybe$Just('Mat')
				} : {
					english: 'April',
					kinyarwanda: $elm$core$Maybe$Just('Mata')
				};
			case 'May':
				return _short ? {
					english: 'May',
					kinyarwanda: $elm$core$Maybe$Just('Gic')
				} : {
					english: 'May',
					kinyarwanda: $elm$core$Maybe$Just('Gicurasi')
				};
			case 'Jun':
				return _short ? {
					english: 'Jun',
					kinyarwanda: $elm$core$Maybe$Just('Kam')
				} : {
					english: 'June',
					kinyarwanda: $elm$core$Maybe$Just('Kamena')
				};
			case 'Jul':
				return _short ? {
					english: 'Jul',
					kinyarwanda: $elm$core$Maybe$Just('Nya')
				} : {
					english: 'July',
					kinyarwanda: $elm$core$Maybe$Just('Nyakanga')
				};
			case 'Aug':
				return _short ? {
					english: 'Aug',
					kinyarwanda: $elm$core$Maybe$Just('Kan')
				} : {
					english: 'August',
					kinyarwanda: $elm$core$Maybe$Just('Kanama')
				};
			case 'Sep':
				return _short ? {
					english: 'Sep',
					kinyarwanda: $elm$core$Maybe$Just('Nze')
				} : {
					english: 'September',
					kinyarwanda: $elm$core$Maybe$Just('Nzeri')
				};
			case 'Oct':
				return _short ? {
					english: 'Oct',
					kinyarwanda: $elm$core$Maybe$Just('Ukw')
				} : {
					english: 'October',
					kinyarwanda: $elm$core$Maybe$Just('Ukwakira')
				};
			case 'Nov':
				return _short ? {
					english: 'Nov',
					kinyarwanda: $elm$core$Maybe$Just('Ugu')
				} : {
					english: 'November',
					kinyarwanda: $elm$core$Maybe$Just('Ugushyingo')
				};
			default:
				return _short ? {
					english: 'Dec',
					kinyarwanda: $elm$core$Maybe$Just('Uku')
				} : {
					english: 'December',
					kinyarwanda: $elm$core$Maybe$Just('Ukuboza')
				};
		}
	});
var $author$project$Translate$translationSet = function (transId) {
	translationSet:
	while (true) {
		switch (transId.$) {
			case 'AcuteMalnutrition':
				return {english: 'Acute Malnutrition', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'AggregatedChildScoreboard':
				return {english: 'Aggregated Child Scoreboard', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'ANCNewborn':
				return {english: 'ANC + Newborn', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'Cell':
				return {english: 'Cell', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'District':
				return {english: 'District', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'Demographics':
				return {english: 'Demographics', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'GenerateReport':
				return {english: 'Generate Report', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'HttpError':
				var val = transId.a;
				return $author$project$Translate$translateHttpError(val);
			case 'InfrastructureEnvironmentWash':
				return {english: 'Infrastructure, Environment & Wash', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'Month':
				var month = transId.a;
				return A2($author$project$Translate$translateMonth, month, false);
			case 'NCDADemographicsItemLabel':
				var item = transId.a;
				switch (item.$) {
					case 'ChildrenUnder2':
						return {english: 'Children under 2', kinyarwanda: $elm$core$Maybe$Nothing};
					case 'NewbornsThisMonth':
						return {english: 'Number of Newborns this month', kinyarwanda: $elm$core$Maybe$Nothing};
					default:
						return {english: 'Low Birth Weigh (Y/N)', kinyarwanda: $elm$core$Maybe$Nothing};
				}
			case 'NCDAAcuteMalnutritionItemLabel':
				var item = transId.a;
				switch (item.$) {
					case 'SevereAcuteMalnutrition':
						return {english: 'Severe Acute Malnutrition', kinyarwanda: $elm$core$Maybe$Nothing};
					case 'ModerateAcuteMalnutrition':
						return {english: 'Moderate Acute Malnutrition', kinyarwanda: $elm$core$Maybe$Nothing};
					default:
						return {english: 'Good Nutrition', kinyarwanda: $elm$core$Maybe$Nothing};
				}
			case 'NCDAStuntingItemLabel':
				var item = transId.a;
				switch (item.$) {
					case 'SevereStunting':
						return {english: 'Severe Stunting', kinyarwanda: $elm$core$Maybe$Nothing};
					case 'ModerateStunting':
						return {english: 'Moderate Stunting', kinyarwanda: $elm$core$Maybe$Nothing};
					default:
						return {english: 'No Stunting', kinyarwanda: $elm$core$Maybe$Nothing};
				}
			case 'NCDAANCNewbornItemLabel':
				var item = transId.a;
				if (item.$ === 'RegularCheckups') {
					return {
						english: 'Regular prenatal and postpartum checkups',
						kinyarwanda: $elm$core$Maybe$Just('Yisuzumishije uko bikwiye atwite na nyuma yo kubyara')
					};
				} else {
					return {
						english: 'Iron during pregnancy',
						kinyarwanda: $elm$core$Maybe$Just('Yafashe umuti wongera amaraso atwite')
					};
				}
			case 'NCDAInfrastructureEnvironmentWashItemLabel':
				var item = transId.a;
				switch (item.$) {
					case 'HasToilets':
						return {
							english: 'Household has toilets',
							kinyarwanda: $elm$core$Maybe$Just('Urugo rufite ubwiherero')
						};
					case 'HasCleanWater':
						return {
							english: 'Household has clean water',
							kinyarwanda: $elm$core$Maybe$Just('Urugo rufite amazi meza')
						};
					case 'HasHandwashingFacility':
						return {
							english: 'Household has handwashing facility',
							kinyarwanda: $elm$core$Maybe$Just('Urugo rufite kandagirukarabe')
						};
					case 'HasKitchenGarden':
						return {
							english: 'Household has kitchen garden',
							kinyarwanda: $elm$core$Maybe$Just('Urugo rufite akarima k\'igikoni')
						};
					default:
						return {
							english: 'Insecticide treated bed nets',
							kinyarwanda: $elm$core$Maybe$Just('Urugo rufite nzitiramibu ikoranye umuti')
						};
				}
			case 'NCDANutritionBehaviorItemLabel':
				var item = transId.a;
				switch (item.$) {
					case 'BreastfedSixMonths':
						return {
							english: 'Breastfed baby for 6 mo without interruption',
							kinyarwanda: $elm$core$Maybe$Just('Konsa umwana amezi 6 utamuvangiye')
						};
					case 'AppropriateComplementaryFeeding':
						return {
							english: 'Appropriate complementary feeding (6-24 mo)',
							kinyarwanda: $elm$core$Maybe$Just('Imfashabere igizwe n’indyo yuzuye (Amezi 6-24)')
						};
					case 'DiverseDiet':
						return {
							english: 'Does the child have a diverse diet?',
							kinyarwanda: $elm$core$Maybe$Just('Umwana afata indyo yuzuye')
						};
					default:
						return {
							english: 'Number of times a child eats a day',
							kinyarwanda: $elm$core$Maybe$Just('Inshuro umwana afata ifunguro ku munsi')
						};
				}
			case 'NCDATargetedInterventionsItemLabel':
				var item = transId.a;
				switch (item.$) {
					case 'FBFGiven':
						return {english: 'FBF', kinyarwanda: $elm$core$Maybe$Nothing};
					case 'TreatmentForAcuteMalnutrition':
						return {
							english: 'Treatment for acute malnutrition (severe or moderate)',
							kinyarwanda: $elm$core$Maybe$Just('Kuvura imiritre mibi  ifatiyeho(Ikabije cg yoroheje)')
						};
					case 'TreatmentForDiarrhea':
						return {
							english: 'Treatment of diarrhea (ORS & Zinc)',
							kinyarwanda: $elm$core$Maybe$Just('Kuvura impiswi(Ukoresheje Zinc cg ORS)')
						};
					case 'SupportChildWithDisability':
						return {
							english: 'Provide support to a child with a disability ',
							kinyarwanda: $elm$core$Maybe$Just('Guha umwana ufite ubumuga ubufasha bwihariye')
						};
					case 'ConditionalCashTransfer':
						return {
							english: 'Receipt of conditional cash transfer e.g. NSDS, VUP',
							kinyarwanda: $elm$core$Maybe$Just('Gufata amafaranga y’inkunga agenerwa umugore utwite n’uwonsa bo mu miryango ikennye (icyiciro cya 1 n’icya 2) – NSDS, VUP')
						};
					default:
						return {
							english: 'Receipt of conditional food items including small livestock',
							kinyarwanda: $elm$core$Maybe$Just('Gufata inkunga z’ingoboka harimo ibiryo n\'amatungo magufi')
						};
				}
			case 'NCDAUniversalInterventionItemLabel':
				var item = transId.a;
				switch (item.$) {
					case 'Immunization':
						return {
							english: 'Immunization',
							kinyarwanda: $elm$core$Maybe$Just('Ikingira')
						};
					case 'VitaminA':
						return {english: 'Vitamin A', kinyarwanda: $elm$core$Maybe$Nothing};
					case 'Deworming':
						return {
							english: 'Deworming',
							kinyarwanda: $elm$core$Maybe$Just('Imiti y\'inzoka')
						};
					case 'OngeraMNP':
						return {
							english: 'Use additional nutrients (Ongera)',
							kinyarwanda: $elm$core$Maybe$Just('Koresha Ongera intungamubiri')
						};
					default:
						return {
							english: 'ECD services provided to child',
							kinyarwanda: $elm$core$Maybe$Just('Umwana yahawe servise n\'ikigo mboneza mikurire')
						};
				}
			case 'NCDAFillTheBlanksItemLabel':
				var item = transId.a;
				switch (item.$) {
					case 'HeightToAge':
						return {
							english: 'Level of stuning using child length mat',
							kinyarwanda: $elm$core$Maybe$Just('Ikigero cyo kugwingira hakoreshejwe agasambi')
						};
					case 'WeightToAge':
						return {
							english: 'Weight',
							kinyarwanda: $elm$core$Maybe$Just('Ibiro')
						};
					case 'MuacValue':
						return {
							english: 'MUAC',
							kinyarwanda: $elm$core$Maybe$Just('Ikizigira')
						};
					default:
						return {
							english: 'Edema',
							kinyarwanda: $elm$core$Maybe$Just('Kubyimba')
						};
				}
			case 'NewSelection':
				return {english: 'New Selection', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'NutritionBehavior':
				return {english: 'Nutrition Behavior', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'Province':
				return {english: 'Province', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'Sector':
				return {english: 'Sector', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'SelectedEntity':
				var entity = transId.a;
				switch (entity.$) {
					case 'EntityDistrict':
						var $temp$transId = $author$project$Translate$District;
						transId = $temp$transId;
						continue translationSet;
					case 'EntitySector':
						var $temp$transId = $author$project$Translate$Sector;
						transId = $temp$transId;
						continue translationSet;
					case 'EntityCell':
						var $temp$transId = $author$project$Translate$Cell;
						transId = $temp$transId;
						continue translationSet;
					default:
						var $temp$transId = $author$project$Translate$Village;
						transId = $temp$transId;
						continue translationSet;
				}
			case 'Stunting':
				return {english: 'Stunting', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'Status':
				return {english: 'Status', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'TargetedInterventions':
				return {english: 'Targeted Interventions', kinyarwanda: $elm$core$Maybe$Nothing};
			case 'Village':
				return {english: 'Village', kinyarwanda: $elm$core$Maybe$Nothing};
			default:
				return {english: 'Univeral Intervention', kinyarwanda: $elm$core$Maybe$Nothing};
		}
	}
};
var $author$project$Translate$translate = F2(
	function (language, transId) {
		var set = $author$project$Translate$translationSet(transId);
		if (language.$ === 'English') {
			return function ($) {
				return $.english;
			}(set);
		} else {
			return A2(
				$elm$core$Maybe$withDefault,
				function ($) {
					return $.english;
				}(set),
				function ($) {
					return $.kinyarwanda;
				}(set));
		}
	});
var $author$project$Utils$WebData$errorString = F2(
	function (language, error) {
		switch (error.$) {
			case 'BadUrl':
				return A2(
					$author$project$Translate$translate,
					language,
					$author$project$Translate$HttpError($author$project$Translate$ErrorBadUrl));
			case 'BadPayload':
				var message = error.a;
				return A2(
					$author$project$Translate$translate,
					language,
					$author$project$Translate$HttpError(
						$author$project$Translate$ErrorBadPayload(message)));
			case 'NetworkError':
				return A2(
					$author$project$Translate$translate,
					language,
					$author$project$Translate$HttpError($author$project$Translate$ErrorNetworkError));
			case 'Timeout':
				return A2(
					$author$project$Translate$translate,
					language,
					$author$project$Translate$HttpError($author$project$Translate$ErrorTimeout));
			default:
				var response = error.a;
				return A2(
					$author$project$Translate$translate,
					language,
					$author$project$Translate$HttpError(
						$author$project$Translate$ErrorBadStatus(
							function () {
								var _v1 = A2(
									$elm$json$Json$Decode$decodeString,
									A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
									response.body);
								if (_v1.$ === 'Ok') {
									var err = _v1.a;
									return err;
								} else {
									return response.status.message;
								}
							}())));
		}
	});
var $author$project$Utils$WebData$viewError = F2(
	function (language, error) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('alert alert-danger')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(
					A2($author$project$Utils$WebData$errorString, language, error))
				]));
	});
var $author$project$Error$View$viewError = F2(
	function (language, error) {
		var apply = function (str) {
			return A2(
				$elm$html$Html$li,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(error.module_ + ('.' + (error.location + ': '))),
						str
					]));
		};
		var _v0 = error.error;
		switch (_v0.$) {
			case 'Decoder':
				var err = _v0.a;
				return apply(
					$elm$html$Html$text(
						$elm$json$Json$Decode$errorToString(err)));
			case 'Http':
				var err = _v0.a;
				return apply(
					A2($author$project$Utils$WebData$viewError, language, err));
			default:
				var txt = _v0.a;
				return apply(
					$elm$html$Html$text(txt));
		}
	});
var $author$project$Error$View$view = F2(
	function (language, errors) {
		return $elm$core$List$isEmpty(errors) ? $author$project$Utils$Html$emptyNode : A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('elm-errors alert debug-errors')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$ul,
					_List_Nil,
					A2(
						$elm$core$List$map,
						$author$project$Error$View$viewError(language),
						errors))
				]));
	});
var $author$project$Translate$GenerateReport = {$: 'GenerateReport'};
var $author$project$Translate$Province = {$: 'Province'};
var $author$project$Pages$Menu$Model$SetGeoLocation = F2(
	function (a, b) {
		return {$: 'SetGeoLocation', a: a, b: b};
	});
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$html$Html$button = _VirtualDom_node('button');
var $author$project$Gizra$Html$emptyNode = $elm$html$Html$text('');
var $pzp1997$assoc_list$AssocList$D = function (a) {
	return {$: 'D', a: a};
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $pzp1997$assoc_list$AssocList$filter = F2(
	function (isGood, _v0) {
		var alist = _v0.a;
		return $pzp1997$assoc_list$AssocList$D(
			A2(
				$elm$core$List$filter,
				function (_v1) {
					var key = _v1.a;
					var value = _v1.b;
					return A2(isGood, key, value);
				},
				alist));
	});
var $author$project$Backend$Entities$EntityId = function (a) {
	return {$: 'EntityId', a: a};
};
var $author$project$Backend$Entities$toEntityId = $author$project$Backend$Entities$EntityId;
var $author$project$Utils$GeoLocation$filterGeoLocationDictByParent = function (parentId) {
	return $pzp1997$assoc_list$AssocList$filter(
		F2(
			function (_v0, geoLocation) {
				return _Utils_eq(
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(parentId)),
					geoLocation.parent);
			}));
};
var $author$project$Backend$Entities$fromEntityId = function (_v0) {
	var a = _v0.a;
	return a;
};
var $author$project$Utils$GeoLocation$GeoLocation = F2(
	function (name, parent) {
		return {name: name, parent: parent};
	});
var $pzp1997$assoc_list$AssocList$remove = F2(
	function (targetKey, _v0) {
		var alist = _v0.a;
		return $pzp1997$assoc_list$AssocList$D(
			A2(
				$elm$core$List$filter,
				function (_v1) {
					var key = _v1.a;
					return !_Utils_eq(key, targetKey);
				},
				alist));
	});
var $pzp1997$assoc_list$AssocList$insert = F3(
	function (key, value, dict) {
		var _v0 = A2($pzp1997$assoc_list$AssocList$remove, key, dict);
		var alteredAlist = _v0.a;
		return $pzp1997$assoc_list$AssocList$D(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(key, value),
				alteredAlist));
	});
var $pzp1997$assoc_list$AssocList$fromList = function (alist) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, result) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($pzp1997$assoc_list$AssocList$insert, key, value, result);
			}),
		$pzp1997$assoc_list$AssocList$D(_List_Nil),
		alist);
};
var $author$project$Utils$GeoLocation$getGeoCells = $pzp1997$assoc_list$AssocList$fromList(
	_Utils_ap(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(4),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Birambo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(3)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(9),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Butereri',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(3)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(17),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Byibuhiro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(3)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(23),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamina',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(3)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(30),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kirabo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(3)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(37),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mwumba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(3)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(43),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ruhanga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(3)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(50),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kiruku',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(49)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(55),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mbirima',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(49)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(64),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyange',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(49)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(72),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyanza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(49)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(83),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Muhaza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(82)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(91),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Muhororo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(82)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(100),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Muramba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(82)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(107),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mutanda',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(82)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(114),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rukore',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(82)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(123),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buheta',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(122)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(128),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kagoma',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(122)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(139),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nganzo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(122)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(153),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rusagara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(122)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(174),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rutabo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(173)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(180),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rutenderi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(173)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(188),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Taba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(173)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(194),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyacyina',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(173)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(208),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rukura',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(173)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(218),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(217)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(224),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karukungu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(217)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(233),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gakindo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(217)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(240),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gashyamba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(217)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(249),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamubuga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(248)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(262),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kidomo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(248)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(271),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mbatabata',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(248)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(280),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rukore',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(248)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(290),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karambo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(289)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(304),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kirebe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(289)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(313),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kanyanza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(289)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(322),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Cyintare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(321)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(325),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ruhinga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(321)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(332),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Sereri',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(321)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(341),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasiza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(321)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(347),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugimbu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(321)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(356),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buyange',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(355)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(366),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gikombe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(355)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(377),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyundo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(355)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(387),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasiho',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(386)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(395),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Munyana',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(386)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(401),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(386)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(407),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Raba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(386)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(416),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Munyana',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(415)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(418),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mutego',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(415)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(424),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nkomane',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(415)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(430),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rutabo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(415)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(436),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rutenderi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(415)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(441),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwamambe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(415)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(446),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gahinga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(415)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(458),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bwenda',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(457)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(460),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasiza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(457)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(466),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gihinga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(457)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(472),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Huro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(457)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(478),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Musagara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(457)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(483),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Musenyi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(457)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(488),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ruganda',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(457)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(494),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwinkuba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(457)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(498),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Busake',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(457)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(508),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bumba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(507)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(516),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gisiza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(507)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(523),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karyango',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(507)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(528),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nganzo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(507)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(534),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Va',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(507)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(541),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mwiyando',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(540)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(547),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(540)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(556),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabatezi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(540)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(564),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kiryamo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(540)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(572),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mubuga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(540)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(582),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buranga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(581)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(590),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gahinga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(581)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(595),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gisozi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(581)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(606),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mucaca',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(581)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(619),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Busoro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(618)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(622),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gikingo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(618)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(629),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Jango',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(618)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(636),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ruli',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(618)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(643),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwesero',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(618)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(655),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rurembo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(654)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(661),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gataba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(654)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(667),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamonyi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(654)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(675),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(654)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(680),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyundo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(654)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(686),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rumbi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(654)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(692),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Burimba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(691)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(696),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Busanane',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(691)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(701),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Joma',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(691)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(707),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kageyo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(691)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(713),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mbogo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(691)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(719),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Razi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(691)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(725),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwankuba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(691)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(731),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Shyombwe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(691)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(738),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwamahwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(737)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(747),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Cyohoha',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(737)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(758),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gitare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(737)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(770),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Butangampundu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(769)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(785),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karengeri',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(769)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(800),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Taba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(769)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(814),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasiza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(813)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(820),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Giko',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(813)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(829),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kayenzi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(813)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(835),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mukoto',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(813)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(843),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyirangarama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(813)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(857),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Busoro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(856)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(863),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Butare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(856)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(869),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gahororo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(856)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(876),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gitumba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(856)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(882),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(856)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(889),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mwumba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(856)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(895),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ndarage',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(856)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(902),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Budakiranya',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(901)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(911),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Migendezo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(901)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(920),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rudogo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(901)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(930),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Burehe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(929)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(934),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Marembo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(929)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(943),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwili',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(929)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(956),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Butunzi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(955)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(963),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karegamazi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(955)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(970),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Marembo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(955)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(976),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rebero',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(955)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(984),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamushenyi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(983)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(987),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kigarama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(983)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(993),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mubuga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(983)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1000),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(983)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1007),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Sayo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(983)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1014),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gitatsa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(983)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1025),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Shengampuli',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1024)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1027),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabuga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1024)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1035),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kigarama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1024)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1040),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kivugiza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1024)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1045),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyamyumba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1024)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1057),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bukoro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1056)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1068),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mushari',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1056)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1076),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ngiramazi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1056)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1084),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rurenge',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1056)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1094),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mugambazi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1093)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1100),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mvuzo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1093)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1108),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bubangu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1093)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1119),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1093)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1116),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bubangu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1115)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1130),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabuga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1129)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1137),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karambo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1129)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1144),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mugote',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1129)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1153),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Munyarwanda',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1129)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1163),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kiyanza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1162)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1170),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mahaza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1162)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1178),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kajevuba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1162)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1190),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buraro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1189)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1199),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bwimo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1189)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1206),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mberuka',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1189)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1212),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mbuye',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1189)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1219),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Taba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1218)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1224),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gako',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1218)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1231),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kirenge',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1218)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1239),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bugaragara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1238)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1248),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kijabagwe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1238)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1255),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Muvumu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1238)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1266),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rubona',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1238)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1275),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rutonde',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1238)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1285),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gahabwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1284)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1290),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Misezero',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1284)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1298),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyirabirori',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1284)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1306),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Taba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1284)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1313),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Barari',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1284)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1323),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kagomasi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1322)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1328),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mwendo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1322)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1335),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ramiro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1322)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1345),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Biryogo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1322)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1355),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabuye',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1322)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1364),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwinume',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1363)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1366),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Juru',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1363)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1373),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabukuba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1363)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1381),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mugorore',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1363)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1390),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Musovu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1363)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1402),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Biharagu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1401)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1410),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Burenge',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1401)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1418),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kampeka',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1401)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1428),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyakayaga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1401)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1436),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Tunda',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1401)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1448),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bushenyi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1447)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1454),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gakomeye',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1447)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1464),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyamigina',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1447)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1475),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rango',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1447)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1486),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugarama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1447)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1506),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gakamba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1505)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1514),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kagenge',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1505)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1524),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kibenga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1505)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1532),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kibirizi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1505)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1540),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mbyo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1505)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1547),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Musenyi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1546)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1553),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyagihunika',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1546)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1563),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rulindo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1546)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1574),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gicaca',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1546)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1598),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bitaba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1597)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1605),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kagasa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1597)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1612),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugunga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1597)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1620),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rurenge',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1597)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1628),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rutonde',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1627)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1630),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gihembe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1627)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1651),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1627)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1668),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ngeruka',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1627)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1676),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyakayenzi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1627)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1692),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Cyugaro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1691)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1700),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kanzenze',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1691)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1709),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kibungo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1691)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1718),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Maranyundo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1717)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1721),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1717)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1733),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyamata y\' umujyi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1717)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1747),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kanazi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1717)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1757),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kayumba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1717)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1771),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ngenda',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1770)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1781),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugando',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1770)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1788),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gihinga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1770)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1796),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabuye',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1770)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1804),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1770)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1816),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ntarama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1815)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1819),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabeza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1815)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1835),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karera',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1815)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1845),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kimaranzara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1815)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1865),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyabagendwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1815)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1878),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kindama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1877)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1880),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ruhuha',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1877)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1886),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bihari',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1877)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1895),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatanga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1877)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1902),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gikundamvura',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1877)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1919),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Batima',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1918)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1930),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kintambwe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1918)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1940),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mazane',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1918)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1944),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nemba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1918)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1954),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nkanga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1918)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1961),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Sharita',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1918)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1965),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabagugu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1964)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1970),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamabuye',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1964)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1974),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nziranziza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1964)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1979),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rebero',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1964)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1984),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rutare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1964)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1993),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyabugogo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1992)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(1996),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ruriba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1992)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2005),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwesero',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1992)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2007),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kigali',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1992)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2025),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mwendo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(1992)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2048),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kinyaga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2047)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2054),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Musave',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2047)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2060),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mvuzo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2047)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2067),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ngara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2047)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2073),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nkuzuzu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2047)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2079),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyabikenke',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2047)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2090),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyagasozi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2047)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2099),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karuruma',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2098)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2111),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyamabuye',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2098)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2128),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyamugari',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2098))))
			]),
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2142),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2141)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2145),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasagara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2141)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2150),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gicaca',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2141)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2155),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kibara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2141)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2160),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Munini',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2141)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2168),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Musezero',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2167)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2177),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ruhango',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2167)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2185),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Akamatamu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2184)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2190),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ngiryi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2184)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2195),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bweramvura',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2184)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2212),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabuye',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2184)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2224),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kidashya',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2184)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2233),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Agateko',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2232)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2241),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buhiza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2232)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2246),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Muko',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2232)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2250),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nkusi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2232)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2256),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyabuliba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2232)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2262),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyakabungo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2232)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2268),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyamitanga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2232)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2274),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamatamu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2273)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2278),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamutwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2273)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2288),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kibaza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2273)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2310),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamukina',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2309)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2320),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kimihurura',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2309)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2330),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugando',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2309)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2336),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bibare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2335)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2347),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kibagabaga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2335)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2362),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyagatovu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2335)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2375),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2374)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2379),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gacuriro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2374)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2386),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasharu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2374)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2391),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kagugu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2374)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2404),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bwiza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2403)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2411),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Cyaruzinge',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2403)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2419),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kibenga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2403)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2431),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Masoro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2403)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2438),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mukuyu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2403)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2446),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rudashya',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2403)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2454),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Butare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2453)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2457),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasanze',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2453)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2464),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasura',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2453)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2472),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatunga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2453)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2480),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Muremure',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2453)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2486),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Sha',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2453)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2493),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Shango',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2453)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2507),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyarutarama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2506)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2513),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rukiri i',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2506)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2521),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rukiri ii',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2506)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2527),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyabisindu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2506)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2538),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabuga i',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2537)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2540),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bisenga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2537)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2545),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasagara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2537)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2556),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabuga ii',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2537)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2564),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kinyana',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2537)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2569),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mbandazi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2537)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2576),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyagahinga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2537)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2584),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ruhanga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2537)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2591),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasabo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2590)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2595),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Indatemwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2590)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2601),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabaliza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2590)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2605),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kacyatwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2590)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2611),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kibenga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2590)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(2615),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kigabiro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(2590))))
			])));
var $author$project$Utils$GeoLocation$getGeoDistricts = $pzp1997$assoc_list$AssocList$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Gakenke',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(736),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Rulindo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1321),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Bugesera',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1320)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1991),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Nyarugenge',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1990)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2046),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Gasabo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1990))))
		]));
var $author$project$Utils$GeoLocation$getGeoProvinces = $pzp1997$assoc_list$AssocList$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1),
			A2($author$project$Utils$GeoLocation$GeoLocation, 'Amajyaruguru', $elm$core$Maybe$Nothing)),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1320),
			A2($author$project$Utils$GeoLocation$GeoLocation, 'Iburasirazuba', $elm$core$Maybe$Nothing)),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1990),
			A2($author$project$Utils$GeoLocation$GeoLocation, 'Umujyi wa kigali', $elm$core$Maybe$Nothing))
		]));
var $author$project$Utils$GeoLocation$getGeoSectors = $pzp1997$assoc_list$AssocList$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(3),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Busengo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(49),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Coko',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(82),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Cyabingo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(122),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Gakenke',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(173),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Gashenyi',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(217),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Janja',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(248),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Kamubuga',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(289),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Karambo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(321),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Kivuruga',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(355),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Mataba',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(386),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Minazi',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(415),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Mugunga',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(457),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Muhondo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(507),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Muyongwe',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(540),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Muzo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(581),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Nemba',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(618),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Ruli',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(654),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Rusasa',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(691),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Rushashi',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(737),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Base',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(769),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Burega',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(813),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Bushoki',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(856),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Buyoga',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(901),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Cyinzuzi',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(929),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Cyungo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(955),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Kinihira',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(983),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Kisaro',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1024),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Masoro',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1056),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Mbogo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1093),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Murambi',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1115),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Murambi',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1129),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Ngoma',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1162),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Ntarabana',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1189),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Rukozo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1218),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Rusiga',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1238),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Shyorongi',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1284),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Tumba',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(736)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1322),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Gashora',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1363),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Juru',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1401),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Kamabuye',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1447),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Mareba',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1505),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Mayange',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1546),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Musenyi',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1597),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Mwogo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1627),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Ngeruka',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1691),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Ntarama',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1717),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Nyamata',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1770),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Nyarugenge',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1815),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Rilima',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1877),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Ruhuha',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1918),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Rweru',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1964),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Shyara',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1321)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(1992),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Kigali',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(1991)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2047),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Bumbogo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2098),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Gatsata',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2141),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Gikomero',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2167),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Gisozi',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2184),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Jabana',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2232),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Jali',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2273),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Kacyiru',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2309),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Kimihurura',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2335),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Kimironko',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2374),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Kinyinya',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2403),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Ndera',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2453),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Nduba',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2506),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Remera',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2537),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Rusororo',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046)))),
			_Utils_Tuple2(
			$author$project$Backend$Entities$toEntityId(2590),
			A2(
				$author$project$Utils$GeoLocation$GeoLocation,
				'Rutunga',
				$elm$core$Maybe$Just(
					$author$project$Backend$Entities$toEntityId(2046))))
		]));
var $author$project$Utils$GeoLocation$getGeoVillages = $pzp1997$assoc_list$AssocList$fromList(
	_Utils_ap(
		_List_fromArray(
			[
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(5),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Birambo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(4)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(6),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gitwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(4)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(7),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kirwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(4)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(8),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyarubande',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(4)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(10),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buhuga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(9)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(11),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Butereri',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(9)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(12),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasakuza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(9)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(13),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kirwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(9)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(14),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rubaga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(9)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(15),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugendabari',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(9)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(16),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwinkuba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(9)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(18),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatoke',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(17)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(19),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamina',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(17)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(20),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(17)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(21),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyagasozi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(17)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(22),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ruboza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(17)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(24),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bunyangezi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(23)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(25),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kajereri',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(23)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(26),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamina',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(23)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(27),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mwendo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(23)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(28),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyarubuye',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(23)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(29),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwankuba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(23)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(31),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasaso',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(30)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(32),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kirabo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(30)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(33),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Munyinya',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(30)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(34),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ngezi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(30)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(35),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rusebeya',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(30)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(36),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Wimfizi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(30)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(38),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabuga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(37)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(39),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamonyi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(37)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(40),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karaba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(37)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(41),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mugunga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(37)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(42),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rutenga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(37)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(44),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bukinga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(43)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(45),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gashirwe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(43)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(46),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabaya',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(43)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(47),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabugiri',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(43)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(48),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rurangara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(43)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(51),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mucumazo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(50)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(52),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ntarabana',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(50)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(53),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyamasuka',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(50)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(54),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rubuguma',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(50)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(78),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buhuri',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(50)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(79),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bukamba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(50)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(80),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bushagashi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(50)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(81),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(50)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(56),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Akanduga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(55)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(57),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Burengo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(55)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(58),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bushyama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(55)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(59),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Matovu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(55)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(60),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mbogo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(55)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(61),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(55)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(62),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwahi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(55)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(63),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Shyunga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(55)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(65),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buhara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(64)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(66),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gaseke',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(64)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(67),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karambo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(64)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(68),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karoli',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(64)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(69),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Musasa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(64)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(70),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ntobwe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(64)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(71),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Vumandi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(64)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(73),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Baramba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(72)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(74),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gikamba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(72)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(75),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gitaba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(72)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(76),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kavumu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(72)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(77),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Tumba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(72)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(84),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buraza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(83)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(85),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Busoga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(83)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(86),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karombero',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(83)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(87),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Muhaza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(83)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(88),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mushirarungu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(83)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(89),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ntaraga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(83)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(90),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rutaramiro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(83)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(92),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Butaraga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(91)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(93),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatoki',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(91)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(94),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatorero',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(91)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(95),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabungwe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(91)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(96),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karenge',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(91)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(97),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Muhororo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(91)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(98),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Musebeya',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(91)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(99),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Tongoburo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(91)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(101),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bukuba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(100)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(102),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gahama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(100)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(103),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(100)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(104),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Musebeya',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(100)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(105),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugaragara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(100)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(106),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwobe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(100)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(108),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Cyabingo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(107)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(109),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gishubi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(107)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(110),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kambare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(107)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(111),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kanyamukenke',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(107)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(112),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mucaca',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(107)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(113),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mutanda',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(107)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(115),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kigote',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(114)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(116),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Muramba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(114)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(117),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murehe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(114)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(118),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyabisika',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(114)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(119),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyamugali',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(114)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(120),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugendabare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(114)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(121),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rukore',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(114)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(124),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mucuro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(123)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(125),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(123)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(126),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ndora',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(123)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(127),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rusebeya',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(123)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(167),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buyagiro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(123)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(168),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(123)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(169),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gihemba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(123)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(170),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gikerera',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(123)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(171),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(123)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(172),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karorero',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(123)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(129),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bukanka',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(128)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(130),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Cyandago',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(128)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(131),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gitenga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(128)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(132),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamatare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(128)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(133),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(128)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(134),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(128)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(135),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Musave',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(128)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(136),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ntobwe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(128)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(137),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rurambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(128)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(138),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rusuri',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(128)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(140),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bwimba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(141),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gahondo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(142),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gashigwe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(143),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gishyinguro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(144),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kaniga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(145),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kanyiramanyana',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(146),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(147),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karehe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(148),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karuganda',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(149),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mbizi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(150),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mbogo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(151),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Muyira',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(152),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ryabazungu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(139)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(154),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Akarugamba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(155),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Busingiryi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(156),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabaya',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(157),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kageyo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(158),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kakinungu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(159),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kivumu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(160),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mazinga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(161),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(162),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Museke',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(163),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyamabuye',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(164),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ruberano',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(165),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Sitwe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(166),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Umujyi wa gakenke',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(153)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(175),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasanzwe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(174)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(176),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabwika',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(174)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(177),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kamurambo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(174)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(178),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kanwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(174)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(179),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rubuga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(174)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(215),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buhira',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(174)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(216),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buturuba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(174)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(181),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gaseke',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(180)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(182),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(180)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(183),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gitaba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(180)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(184),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabere',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(180)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(185),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabugomba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(180)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(186),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kibara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(180)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(187),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murambo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(180)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(189),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Busaro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(188)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(190),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bushita',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(188)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(191),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasharu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(188)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(192),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gihanga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(188)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(193),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kangomba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(188)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(196),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kanteko',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(188)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(198),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(188)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(200),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mwisha',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(188)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(201),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rutenderi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(188)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(195),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bwiyando',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(194)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(197),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gashinge',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(194)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(199),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kadehero',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(194)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(202),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Masoro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(194)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(203),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mukira',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(194)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(204),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyamure',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(194)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(205),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugarama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(194)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(206),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugendabari',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(194)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(207),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ruhore',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(194)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(209),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gahihi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(208)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(210),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gikoro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(208)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(211),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(208)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(212),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kirambo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(208)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(213),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murandi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(208)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(214),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyamataha',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(208)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(219),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kinoko',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(218)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(220),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Murambi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(218)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(221),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mwanza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(218)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(222),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyabushishiri',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(218)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(223),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyagisozi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(218)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(246),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buhanga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(218)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(247),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gitega',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(218)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(225),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buhimbi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(224)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(226),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Cyifuzo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(224)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(227),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gitaba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(224)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(228),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(224)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(229),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mugandu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(224)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(230),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugeshi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(224)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(231),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rusasa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(224)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(232),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rutake',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(224)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(234),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bukerera',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(233)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(235),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bunyironko',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(233)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(236),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabusoro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(233)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(237),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kibonwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(233)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(238),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rubona',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(233)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(239),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rurumbya',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(233)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(241),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Burega',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(240)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(242),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatongo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(240)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(243),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gitovu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(240)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(244),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyabikenke',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(240)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(245),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwampali',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(240)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(250),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasebeya',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(249)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(251),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gashishi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(249)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(252),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gitwe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(249)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(253),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabuye',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(249)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(254),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kanshenge',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(249)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(255),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kanyirantege',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(249)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(256),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Marira',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(249)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(257),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyarungu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(249)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(258),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Raro',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(249)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(259),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugari',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(249)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(260),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ruhehe',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(249)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(261),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Runeka',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(249)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(263),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bucyaba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(262)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(264),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bugogo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(262)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(265),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kidomo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(262)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(266),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kintobo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(262)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(267),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Njugi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(262)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(268),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Nyamusongati',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(262)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(269),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rugeshi',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(262)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(270),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rutagara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(262)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(272),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Buhinda',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(271)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(273),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(271)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(274),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Horero',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(271)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(275),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabyaza',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(271)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(276),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karingorera',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(271)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(277),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mbatabata',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(271)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(278),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mwasha',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(271)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(279),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Ryabirere',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(271)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(281),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kabutwa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(280)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(282),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Karangara',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(280)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(283),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kinyababa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(280)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(284),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rungu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(280)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(285),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rusasa',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(280)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(286),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rusumo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(280)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(287),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Rwata',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(280)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(288),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Taba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(280)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(291),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bataga',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(290)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(292),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bumbeja',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(290)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(293),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Bushumba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(290)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(294),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Cyumba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(290)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(295),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gasovu',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(290)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(296),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatare',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(290)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(297),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gatorero',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(290)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(298),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Gishingo',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(290)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(299),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Kigarama',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(290)))),
				_Utils_Tuple2(
				$author$project$Backend$Entities$toEntityId(300),
				A2(
					$author$project$Utils$GeoLocation$GeoLocation,
					'Mugamba',
					$elm$core$Maybe$Just(
						$author$project$Backend$Entities$toEntityId(290))))
			]),
		_Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(301),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyiramuhimba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(290)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(302),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rwamiko',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(290)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(303),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ryarurimbura',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(290)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(305),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bukondo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(304)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(306),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bukunga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(304)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(307),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bukweto',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(304)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(308),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabuye',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(304)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(309),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kavumu',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(304)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(310),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mubuga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(304)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(311),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mwiyanike',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(304)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(312),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyabigugu',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(304)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(314),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gatembe',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(313)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(315),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabuhunu',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(313)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(316),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabutare',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(313)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(317),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Karambi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(313)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(318),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Karenge',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(313)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(319),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Marembo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(313)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(320),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyiramisabike',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(313)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(323),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bigogwe',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(322)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(324),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Buhuga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(322)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(339),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Cyintare',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(322)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(340),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyarubuye',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(322)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(326),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kamomo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(325)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(327),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kavumu',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(325)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(328),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kintarure',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(325)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(329),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Munyege',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(325)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(330),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rugeshi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(325)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(331),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rwakirari',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(325)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(354),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Buranga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(325)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(333),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Buhayo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(332)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(334),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabara',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(332)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(335),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kivuruga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(332)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(336),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Masoro',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(332)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(337),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Musekera',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(332)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(338),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ngarama',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(332)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(342),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bushoka',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(341)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(343),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabuhoma',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(341)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(344),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kamwumba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(341)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(345),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nturo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(341)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(346),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyarungu',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(341)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(348),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gasave',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(347)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(349),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Karuhunge',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(347)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(350),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mugali',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(347)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(351),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rurambo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(347)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(352),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rutamba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(347)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(353),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rwamabare',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(347)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(357),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gabiro',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(356)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(358),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gashingiro',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(356)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(359),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabeza',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(356)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(360),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kanamo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(356)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(361),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Karambi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(356)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(362),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mubuga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(356)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(363),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyamiyaga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(356)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(364),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rugendabari',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(356)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(365),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ryarugema',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(356)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(367),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bugari',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(366)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(368),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bweramana',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(366)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(369),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gashyushya',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(366)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(370),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gatovu',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(366)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(371),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Muhororo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(366)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(372),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Munini',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(366)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(373),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Muyaga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(366)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(374),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyangoma',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(366)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(375),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ruganda',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(366)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(376),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ruhanga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(366)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(378),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gihita',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(377)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(379),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gitaba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(377)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(380),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabuyora',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(377)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(381),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kagando',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(377)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(382),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Karambi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(377)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(383),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mataba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(377)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(384),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mwanza',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(377)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(385),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nkurazo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(377)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(388),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gahombo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(387)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(389),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gahunda',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(387)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(390),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gasangwa',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(387)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(391),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gihinga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(387)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(392),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabarima',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(387)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(393),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kigeyo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(387)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(394),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mbogo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(387)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(396),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gihororo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(395)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(397),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gitwa',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(395)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(398),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kanka',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(395)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(399),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kivuba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(395)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(400),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyabitare',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(395)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(402),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gisovu',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(401)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(403),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabuga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(401)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(404),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Musave',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(401)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(405),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyanza',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(401)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(406),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyarubuye',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(401)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(408),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bukonde',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(407)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(409),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gaharo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(407)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(410),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gitaragwe',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(407)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(411),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Munihi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(407)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(412),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mutara',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(407)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(413),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ndegamire',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(407)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(414),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Sarabuye',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(407)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(417),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rwezamenyo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(416)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(453),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Cyarubayi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(416)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(454),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Karambi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(416)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(455),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Muhororo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(416)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(456),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nturo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(416)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(419),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kamasanze',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(418)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(420),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kamunyana',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(418)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(421),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Karambo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(418)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(422),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nganzo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(418)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(423),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rutaraga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(418)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(425),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabuga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(424)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(426),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kanaba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(424)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(427),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nemba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(424)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(428),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyagasozi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(424)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(429),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rusebeya',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(424)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(431),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gacemeri',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(430)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(432),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gasovu',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(430)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(433),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gatonde',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(430)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(434),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabuhoro',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(430)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(435),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Muhororo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(430)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(437),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kiraro',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(436)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(438),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyakazenga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(436)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(439),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyundo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(436)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(440),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rubona',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(436)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(442),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Biraro',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(441)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(443),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bushoka',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(441)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(444),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gashubi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(441)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(445),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabiganda',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(441)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(448),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kanyinya',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(441)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(447),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Cyinama',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(446)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(449),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Giheta',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(446)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(450),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyagahondo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(446)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(451),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyakagezi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(446)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(452),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rwimpiri',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(446)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(459),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nketsi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(458)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(503),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gahama',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(458)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(504),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gatare',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(458)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(505),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gitaba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(458)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(506),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kimanama',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(458)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(461),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gahabwa',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(460)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(462),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gahinga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(460)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(463),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gahondo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(460)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(464),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gasiza',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(460)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(465),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabeza',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(460)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(467),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Base',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(466)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(468),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gihinga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(466)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(469),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Karehe',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(466)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(470),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Samuduha',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(466)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(471),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Taba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(466)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(473),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Cura',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(472)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(474),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gitwa',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(472)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(475),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Huro',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(472)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(476),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabuga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(472)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(477),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rubona',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(472)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(479),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Akara',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(478)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(480),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Cyenda',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(478)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(481),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Giteme',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(478)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(482),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Karobagire',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(478)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(484),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Buhinya',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(483)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(485),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gakuyu',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(483)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(486),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kigali',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(483)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(487),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Musenyi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(483)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(489),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gisozi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(488)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(490),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kinyonzo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(488)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(491),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mubuga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(488)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(492),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ranzi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(488)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(493),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ruganda',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(488)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(495),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Cyimbogo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(494)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(496),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kanyana',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(494)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(497),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ruhorobero',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(494)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(499),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Busake',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(498)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(500),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gikikira',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(498)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(501),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kibirizi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(498)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(502),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyakabanda',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(498)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(509),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bumba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(508)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(510),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Buzu',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(508)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(511),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gikoro',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(508)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(512),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gitovu',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(508)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(513),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gitwe',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(508)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(514),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mataba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(508)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(515),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Shiru',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(508)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(517),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gitanda',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(516)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(518),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabingo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(516)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(519),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kiyebe',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(516)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(520),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Muramba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(516)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(521),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ruhoko',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(516)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(522),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Sanzare',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(516)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(524),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gikombe',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(523)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(525),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kibingo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(523)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(526),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mahaha',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(523)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(527),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mugera',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(523)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(529),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Muhororo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(528)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(530),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nganzo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(528)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(531),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ngoma',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(528)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(532),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyarubuye',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(528)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(533),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Vugangoma',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(528)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(535),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bukwera',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(534)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(536),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Businde',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(534)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(537),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gikombe',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(534)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(538),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mutoyi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(534)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(539),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ranzi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(534)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(542),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kagano',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(541)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(543),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Muguguri',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(541)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(544),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyagasozi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(541)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(545),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rubayo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(541)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(546),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Ruhondo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(541)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(579),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gitabi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(541)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(580),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gitoke',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(541)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(548),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bitaba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(547)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(549),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Cyinturo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(547)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(550),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gacaca',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(547)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(551),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gihororo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(547)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(552),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabere',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(547)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(553),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mafubo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(547)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(554),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyagahondo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(547)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(555),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyarubande',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(547)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(557),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Curugusi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(556)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(558),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gasave',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(556)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(559),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gitabi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(556)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(560),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabatezi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(556)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(561),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kasheshe',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(556)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(562),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Runyinya',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(556)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(563),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rusororo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(556)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(565),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Akamagaju',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(564)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(566),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gahondo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(564)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(567),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Munyinya',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(564)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(568),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Murambi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(564)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(569),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rugarama',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(564)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(570),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rugege',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(564)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(571),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Sezuku',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(564)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(573),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Butambwe',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(572)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(574),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kanini',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(572)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(575),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kavuza',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(572)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(576),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mubuga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(572)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(577),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mwirika',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(572)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(578),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mwurire',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(572)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(583),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Buranga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(582)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(584),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Burego',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(582)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(585),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Butare',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(582)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(586),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kanyansyo',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(582)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(587),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Muganwa',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(582)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(588),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mukaka',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(582)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(589),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Rukoji',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(582)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(591),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bitare',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(590)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(592),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Bukurura',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(590)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(593),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabaya',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(590)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(594),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kilimbi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(590)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(596),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gisagara',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(595)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(597),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kabushara',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(595)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(598),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kamatete',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(595)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(599),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kanama',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(595)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(600),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kanunga',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(595)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(601),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kanzoka',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(595)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(602),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Karukara',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(595)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(603),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Kirehe',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(595)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(604),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Mushubi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(595)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(605),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Nyamyumba',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(595)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(607),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Cyahafi',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(606)))),
					_Utils_Tuple2(
					$author$project$Backend$Entities$toEntityId(608),
					A2(
						$author$project$Utils$GeoLocation$GeoLocation,
						'Gatare',
						$elm$core$Maybe$Just(
							$author$project$Backend$Entities$toEntityId(606))))
				]),
			_Utils_ap(
				_List_fromArray(
					[
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(609),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kabingo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(606)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(610),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kabuye',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(606)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(611),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kamuvunyi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(606)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(612),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kiruhura',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(606)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(613),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kiryamo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(606)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(614),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Munyege',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(606)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(615),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Musange',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(606)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(616),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Ntakabavu',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(606)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(617),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyamiyaga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(606)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(620),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nkoto',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(619)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(621),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rugaragara',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(619)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(649),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Congoli',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(619)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(650),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Cyoganyoni',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(619)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(651),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gitaba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(619)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(652),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kabare',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(619)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(653),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kibirizi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(619)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(623),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bushoka',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(622)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(624),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatwa',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(622)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(625),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kabingo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(622)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(626),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karango',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(622)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(627),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyamugari',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(622)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(628),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rumasa',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(622)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(630),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatagara',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(629)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(631),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gihura',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(629)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(632),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gitonde',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(629)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(633),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kinyonzo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(629)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(634),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mubuga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(629)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(635),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Murehe',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(629)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(637),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bariza',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(636)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(638),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gahondo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(636)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(639),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gataba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(636)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(640),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mugambazi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(636)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(641),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Ngayake',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(636)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(642),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyakarambi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(636)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(644),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatare',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(643)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(645),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gisizi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(643)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(646),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mabago',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(643)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(647),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mugwato',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(643)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(648),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyarunyinya',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(643)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(656),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bushoka',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(655)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(657),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mazinga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(655)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(658),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Murori',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(655)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(659),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyakabungo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(655)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(660),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rugamba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(655)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(662),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bumonyo a',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(661)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(663),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gahama',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(661)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(664),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gataba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(661)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(665),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kebero',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(661)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(666),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kibaya',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(661)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(668),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Burinda',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(667)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(669),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gakindo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(667)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(670),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gapfura',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(667)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(671),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gitwe',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(667)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(672),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kidomo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(667)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(673),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyagahama',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(667)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(674),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rurambi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(667)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(676),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Buharabuye',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(675)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(677),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karuhunge',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(675)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(678),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kirehe',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(675)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(679),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyange',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(675)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(681),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bukingo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(680)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(682),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bumonyo b',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(680)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(683),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gisovu',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(680)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(684),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyundo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(680)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(685),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Tane',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(680)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(687),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bukiza',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(686)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(688),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Buyora',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(686)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(689),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bwanamo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(686)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(690),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Ninda',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(686)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(693),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kabuye',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(692)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(694),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kara',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(692)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(695),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kivumu',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(692)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(697),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gisenyi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(696)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(698),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gisiza',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(696)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(699),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kanzuki',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(696)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(700),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyakagezi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(696)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(702),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kineza',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(701)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(703),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mataba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(701)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(704),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mwifuzo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(701)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(705),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyagasozi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(701)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(706),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rugarama',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(701)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(708),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kabeza',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(707)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(709),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kabona',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(707)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(710),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karambi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(707)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(711),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Murambi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(707)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(712),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nganzo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(707)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(714),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bushoka',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(713)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(715),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Buzoza',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(713)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(716),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gisanze',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(713)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(717),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gitongo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(713)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(718),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyabitare',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(713)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(720),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gahinga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(719)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(721),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gikongoro',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(719)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(722),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kirwa',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(719)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(723),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nkoto',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(719)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(724),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyangoyi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(719)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(726),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Giheta',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(725)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(727),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karushashi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(725)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(728),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Ngambi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(725)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(729),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Ruganda',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(725)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(730),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rwamabega',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(725)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(732),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatare',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(731)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(733),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatwa',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(731)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(734),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gihororo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(731)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(735),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Murara',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(731)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(739),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Base',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(738)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(740),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Cyondo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(738)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(741),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gitovu',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(738)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(742),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kabahama',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(738)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(743),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kabeza',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(738)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(744),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karambi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(738)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(745),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kiruli',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(738)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(746),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mutima',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(738)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(748),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bukangano',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(747)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(749),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Buramba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(747)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(750),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gihemba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(747)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(751),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gitwa',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(747)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(752),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kabingo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(747)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(753),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kabuga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(747)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(754),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Musenyi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(747)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(755),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mushongi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(747)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(756),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyangoyi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(747)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(757),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rubanda',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(747)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(759),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bushyiga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(758)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(760),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatete',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(758)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(761),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gihora',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(758)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(762),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gisiza',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(758)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(763),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kirwa',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(758)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(764),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mugenda i',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(758)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(765),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mugenda ii',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(758)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(766),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyamugali',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(758)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(767),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rugaragara',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(758)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(768),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rugerero',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(758)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(771),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gacyamo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(772),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gashinge',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(773),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karambi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(774),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karugaju',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(775),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kerera',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(776),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kibiraro',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(777),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kigabiro',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(778),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kigarama',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(779),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kisigiro',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(780),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mayaga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(781),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Muduha',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(782),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Muhondo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(783),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyamiyaga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(784),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Runyinya',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(770)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(786),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bugoboka',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(787),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Byerwa',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(788),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gasare',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(789),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gasharu',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(790),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gashinge',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(791),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatete',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(792),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kantabo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(793),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kanunga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(794),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kizenga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(795),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kiziba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(796),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mataba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(797),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mitabi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(798),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mukarange',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(799),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rwamiko',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(785)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(801),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bugarama',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(800)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(802),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Cyinzuzi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(800)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(803),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gasango',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(800)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(804),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kiboha',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(800)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(805),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kivomo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(800)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(806),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mwenene',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(800)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(807),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mwite',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(800)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(808),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Ngange',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(800)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(809),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyagisozi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(800)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(810),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rubara',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(800)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(811),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rusine',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(800)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(812),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Ryinzovu',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(800)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(815),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gitwa',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(814)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(816),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karambi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(814)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(817),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Remera',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(814)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(818),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Ruhanga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(814)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(819),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rulindo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(814)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(854),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Budaha',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(814)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(855),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Buhande',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(814)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(821),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Buramira',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(820)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(822),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Cyiri',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(820)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(823),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gashiru',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(820)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(824),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karambo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(820)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(825),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kigamba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(820)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(826),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kivomo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(820)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(827),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Ngarama',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(820)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(828),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rugote',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(820)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(830),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gitaba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(829)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(831),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Muduha',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(829)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(832),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Murambo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(829)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(833),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rebero',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(829)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(834),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rwanzu',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(829)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(836),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Buvumo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(835)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(837),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Buyogoma',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(835)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(838),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatare',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(835)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(839),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Marembo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(835)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(840),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Muko',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(835)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(841),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mukoto',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(835)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(842),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rusave',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(835)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(844),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bubiro',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(843)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(845),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Byimana',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(843)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(846),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatenga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(843)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(847),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gifuba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(843)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(848),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karambi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(843)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(849),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyenyeri',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(843)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(850),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyirangarama',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(843)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(851),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Remera',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(843)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(852),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Tare',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(843)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(853),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Terambere',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(843)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(858),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gashana',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(857)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(859),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatwa',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(857)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(860),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karambo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(857)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(861),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kibanda',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(857)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(862),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rugarama',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(857)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(864),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gasave',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(863)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(865),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Giko',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(863)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(866),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kankanga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(863)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(867),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karambi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(863)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(868),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Ryanyirakayobe',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(863)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(870),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Bunyana',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(869)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(871),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatare',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(869)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(872),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatenderi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(869)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(873),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gipfundo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(869)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(874),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gitabura',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(869)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(875),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Shagasha',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(869)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(877),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gitaba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(876)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(878),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Munini',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(876)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(879),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyarubuye',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(876)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(880),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Remera',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(876)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(881),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rutabo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(876)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(883),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Cyasenge',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(882)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(884),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kajeneni',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(882)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(885),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karambi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(882)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(886),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karambo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(882)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(887),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kavumo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(882)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(888),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kigarama',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(882)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(890),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gakoma',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(889)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(891),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Mataba',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(889)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(892),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Murambo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(889)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(893),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyamwiza',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(889)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(894),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyarubuye',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(889)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(896),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gahondo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(895)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(897),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gikingo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(895)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(898),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kagozi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(895)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(899),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Karambi',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(895)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(900),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kimagali',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(895)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(903),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gatagara',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(902)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(904),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Gihinga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(902)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(905),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kamatongo',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(902)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(906),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kanyoni',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(902)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(907),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kavumu',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(902)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(908),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Kigarama',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(902)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(909),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Nyakabanga',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(902)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(910),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Rugaragara',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(902)))),
						_Utils_Tuple2(
						$author$project$Backend$Entities$toEntityId(912),
						A2(
							$author$project$Utils$GeoLocation$GeoLocation,
							'Cyanya',
							$elm$core$Maybe$Just(
								$author$project$Backend$Entities$toEntityId(911))))
					]),
				_Utils_ap(
					_List_fromArray(
						[
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(913),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gitabage',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(911)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(914),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karambo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(911)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(915),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Marembo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(911)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(916),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ngabitsinze',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(911)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(917),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyamugali',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(911)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(918),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Remera',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(911)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(919),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rusagara',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(911)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(921),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gasekabuye',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(920)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(922),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gaseke',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(920)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(923),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gasizi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(920)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(924),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gihuke',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(920)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(925),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kirambo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(920)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(926),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Munini',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(920)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(927),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Munoga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(920)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(928),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Musenyi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(920)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(931),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kibogora',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(930)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(932),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyagatovu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(930)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(933),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Sove',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(930)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(951),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gitandi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(930)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(952),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karambo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(930)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(953),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karengeri',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(930)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(954),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kibande',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(930)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(935),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Buyaga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(934)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(936),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gahinga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(934)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(937),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kibuye',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(934)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(938),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kidomo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(934)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(939),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Murambo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(934)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(940),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nganzo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(934)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(941),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rugaragara',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(934)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(942),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rusayu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(934)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(944),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabanda',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(943)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(945),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karambi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(943)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(946),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kirwa',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(943)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(947),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kivumu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(943)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(948),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nturo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(943)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(949),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyabisasa',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(943)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(950),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Sakara',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(943)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(957),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Akamiyove',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(956)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(958),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Barayi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(956)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(959),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Bunahi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(956)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(960),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gisekuru',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(956)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(961),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kinihira',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(956)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(962),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ndorandi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(956)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(964),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Buhita',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(963)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(965),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Bwishya',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(963)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(966),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gatembe',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(963)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(967),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Magezi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(963)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(968),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Mutoyi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(963)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(969),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ntunguru',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(963)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(971),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Buhunde',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(970)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(972),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Cyogo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(970)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(973),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gatare',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(970)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(974),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kigali',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(970)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(975),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kiyebe',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(970)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(977),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabuga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(976)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(978),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karambi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(976)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(979),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kirwa',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(976)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(980),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ndusu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(976)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(981),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rugundu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(976)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(982),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Taba',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(976)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(985),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Songa',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(984)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(986),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Wamahoro',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(984)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1019),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gakenke',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(984)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1020),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gatete',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(984)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1021),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gatovu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(984)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1022),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabeza',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(984)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1023),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karambi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(984)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(988),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gaseke',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(987)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(989),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gasharu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(987)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(990),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyantabo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(987)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(991),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Runyinya',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(987)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(992),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rwintare',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(987)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(994),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gako',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(993)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(995),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kibuye',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(993)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(996),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kirenge',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(993)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(997),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Murambi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(993)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(998),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyakarekare',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(993)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(999),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rutabo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(993)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1001),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Akamanama',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1000)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1002),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gishinge',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1000)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1003),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karambi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1000)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1004),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kibingwe',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1000)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1005),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Mugomero',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1000)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1006),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ryarubuguza',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1000)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1008),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Cyasuri',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1007)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1009),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kibanda',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1007)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1010),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyamiyaga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1007)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1011),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rugarama',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1007)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1012),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rusongati',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1007)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1013),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rusumo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1007)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1015),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabere',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1014)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1016),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ndago',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1014)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1017),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ruberano',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1014)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1018),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rwili',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1014)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1026),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Umutagata',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1025)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1051),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Agasharu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1025)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1052),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Amataba',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1025)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1053),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyabinyana',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1025)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1054),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rusine',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1025)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1055),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Umubuga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1025)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1028),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gisiza',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1027)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1029),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kanunga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1027)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1030),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karambi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1027)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1031),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kigarama',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1027)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1032),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyakibande',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1027)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1033),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyakizu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1027)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1034),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rubaya',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1027)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1036),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gacyamo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1035)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1037),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Marenge',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1035)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1038),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyakabungo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1035)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1039),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rukurazo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1035)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1041),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gasenga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1040)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1042),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Musega',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1040)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1043),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyarurembo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1040)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1044),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rebero',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1040)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1046),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabeza',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1045)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1047),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabuga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1045)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1048),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kigomwa',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1045)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1049),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Marembo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1045)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1050),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rusenyi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1045)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1058),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Buhira',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1057)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1059),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Bukoro',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1057)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1060),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gasama',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1057)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1061),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gihonga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1057)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1062),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kalindi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1057)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1063),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kibamba',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1057)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1064),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kibaya',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1057)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1065),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kinini ya mbogo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1057)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1066),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ruhanya',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1057)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1067),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rwambogo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1057)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1069),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Bukongi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1068)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1070),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Buraro',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1068)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1071),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Buyanja',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1068)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1072),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gitaba',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1068)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1073),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nkurura',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1068)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1074),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyakabuye',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1068)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1075),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rwambogo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1068)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1077),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gasovu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1076)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1078),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gikombe',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1076)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1079),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Yaramba',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1076)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1080),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kibungo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1076)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1081),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Muhora',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1076)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1082),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyakabembe',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1076)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1083),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gisha',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1076)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1085),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gakoma',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1084)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1086),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gicumbi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1084)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1087),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gitaba',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1084)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1088),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karehe',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1084)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1089),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Munini',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1084)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1090),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ruhondo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1084)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1091),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rurenge',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1084)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1092),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rutonde',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1084)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1095),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gahama',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1094)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1096),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gashinge',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1094)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1097),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kigarama',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1094)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1098),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyarurembo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1094)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1099),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ruri',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1094)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1127),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Amahoro',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1094)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1128),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Buliza',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1094)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1101),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Iraro',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1100)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1102),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabeza',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1100)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1103),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabuga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1100)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1104),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Munyinya',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1100)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1105),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Mutabo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1100)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1106),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ntyaba',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1100)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1107),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rurama',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1100)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1109),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gashubi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1108)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1110),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karambo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1108)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1111),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karwa',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1108)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1112),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Mayange',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1108)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1113),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyagisozi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1108)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1114),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rebero',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1108)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1117),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ruhunga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1116)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1118),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Taba',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1116)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1120),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Agatare',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1119)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1121),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Akarambi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1119)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1122),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Amataba',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1119)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1123),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gisiza',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1119)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1124),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabeza',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1119)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1125),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karambo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1119)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1126),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kigarama',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1119)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1131),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gatete',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1130)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1132),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kagarama',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1130)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1133),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kirambo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1130)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1134),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kiruli',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1130)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1135),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyabuko',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1130)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1136),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rubona',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1130)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1138),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Butare',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1137)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1139),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Jyambere',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1137)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1140),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kagwa',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1137)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1141),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karambi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1137)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1142),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Marebe',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1137)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1143),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyakagezi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1137)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1145),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Cyabasigi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1144)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1146),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kiboha',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1144)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1147),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kigina',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1144)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1148),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Mwishya',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1144)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1149),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyakibyeyi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1144)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1150),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Riryi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1144)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1151),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rukoma',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1144)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1152),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Sakara',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1144)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1154),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Busizi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1153)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1155),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gaseke',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1153)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1156),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kirungu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1153)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1157),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Muyange',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1153)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1158),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ngaru',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1153)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1159),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyaruvumu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1153)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1160),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rushayu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1153)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1161),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rushubi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1153)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1164),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kivubwe',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1163)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1165),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kiyanza i',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1163)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1166),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nombe',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1163)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1167),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyagisozi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1163)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1168),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyamurema',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1163)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1169),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyarurama',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1163)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1187),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gatobotobo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1163)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1188),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabirizi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1163)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1171),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Burambi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1170)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1172),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gitwa',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1170)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1173),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kamuhororo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1170)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1174),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Karera',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1170)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1175),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kayenzi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1170)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1176),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kibeho',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1170)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1177),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rusekabuye',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1170)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1179),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Bikamba',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1178)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1180),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Cyamutara',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1178)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1181),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gitambi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1178)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1182),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kazi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1178)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1183),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyakambu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1178)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1184),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyarubuye',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1178)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1185),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rukore',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1178)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1186),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rusasa',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1178)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1191),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabgayi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1190)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1192),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabingo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1190)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1193),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kamiyove',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1190)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1194),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kivomo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1190)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1195),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Murwa',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1190)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1196),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyenyeri',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1190)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1197),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Rukingu',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1190)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1198),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Shyondwe',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1190)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1200),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Bushyana',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1199)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1201),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gatiba',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1199)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1202),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gatwa',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1199)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1203),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kadendegeri',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1199)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1204),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kavumo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1199)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1205),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Mwana',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1199)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1207),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gahwazi',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1206)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1208),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Gakubo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1206)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1209),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kabera',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1206)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1210),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Mataba',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1206)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1211),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Mutungo',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1206)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1213),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Kibare',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1212)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1214),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Mujebe',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1212)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1215),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Musave',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1212)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1216),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Nyarusebeya',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1212)))),
							_Utils_Tuple2(
							$author$project$Backend$Entities$toEntityId(1217),
							A2(
								$author$project$Utils$GeoLocation$GeoLocation,
								'Ruhanga',
								$elm$core$Maybe$Just(
									$author$project$Backend$Entities$toEntityId(1212))))
						]),
					_Utils_ap(
						_List_fromArray(
							[
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1220),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Karambi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1219)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1221),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Karenge',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1219)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1222),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kingazi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1219)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1223),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyakarama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1219)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1236),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Bitare',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1219)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1237),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gahondo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1219)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1225),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gifumba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1224)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1226),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabunigu',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1224)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1227),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabuye',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1224)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1228),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nkanga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1224)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1229),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ntakara',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1224)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1230),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rwintare',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1224)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1232),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kigarama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1231)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1233),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kinini-rusiga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1231)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1234),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ntaruka',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1231)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1235),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rebero',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1231)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1240),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gatimba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1239)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1241),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gatwa',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1239)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1242),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gisiza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1239)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1243),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabaraza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1239)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1244),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kigarama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1239)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1245),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kiziranyenzi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1239)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1246),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyakaruri',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1239)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1247),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyarushinya',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1239)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1249),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gaseke',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1248)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1250),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabagabaga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1248)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1251),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabakene',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1248)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1252),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyamugari',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1248)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1253),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rimwe',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1248)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1254),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rugendabari',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1248)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1256),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Cyikera',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1255)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1257),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kagunda',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1255)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1258),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Karama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1255)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1259),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kavoma',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1255)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1260),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kirurumo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1255)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1261),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kivili',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1255)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1262),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mukumba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1255)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1263),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Muvumu',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1255)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1264),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyabubare',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1255)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1265),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ruhanga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1255)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1267),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Bwimo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1266)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1268),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gishyita',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1266)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1269),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kigali',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1266)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1270),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ngona',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1266)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1271),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyabitare',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1266)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1272),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyarunyinya',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1266)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1273),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyarusange',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1266)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1274),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rwahi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1266)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1276),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Bugarura',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1275)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1277),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mwagiro',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1275)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1278),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ngendo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1275)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1279),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyabisindu',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1275)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1280),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyabyondo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1275)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1281),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyamirembe',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1275)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1282),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rutonde',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1275)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1283),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rweya',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1275)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1286),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kagusa',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1285)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1287),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mafene',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1285)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1288),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Munyinya',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1285)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1289),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rushaki',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1285)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1319),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabuga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1285)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1291),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kanaba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1290)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1292),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Karambi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1290)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1293),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kavumu',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1290)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1294),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Marembo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1290)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1295),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Misezero',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1290)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1296),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rurambo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1290)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1297),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Taba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1290)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1299),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Bukinga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1298)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1300),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gatare',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1298)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1301),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gatsinde',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1298)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1302),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gihanga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1298)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1303),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Murambi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1298)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1304),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rugando',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1298)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1305),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rusura',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1298)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1307),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kamuragi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1306)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1308),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mwili',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1306)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1309),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nkinda',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1306)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1310),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyirambuga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1306)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1311),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyirataba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1306)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1312),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ruvumba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1306)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1314),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gaseke',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1313)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1315),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gashoro',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1313)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1316),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Karambi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1313)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1317),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kigarama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1313)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1318),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rukore',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1313)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1324),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kiruhura',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1323)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1325),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kuruganda',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1323)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1326),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Runzenze',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1323)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1327),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rushubi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1323)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1361),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Akagako',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1323)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1362),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kagomasi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1323)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1329),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gaharwa',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1328)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1330),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gisenyi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1328)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1331),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kayovu',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1328)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1332),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ruhanga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1328)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1333),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ruhanura',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1328)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1334),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rutanga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1328)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1336),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Dihiro',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1335)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1337),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kagasa i',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1335)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1338),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kagasa ii',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1335)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1339),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Karusine i',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1335)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1340),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Karusine ii',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1335)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1341),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Migina',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1335)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1342),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Munyinya',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1335)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1343),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rweru i',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1335)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1344),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rweru ii',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1335)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1346),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Bidudu',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1345)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1347),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Biryogo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1345)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1348),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Buhoro',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1345)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1349),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gihanama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1345)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1350),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kagarama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1345)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1351),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kanyonyomba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1345)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1352),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Karutete',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1345)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1353),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kivugiza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1345)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1354),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rugunga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1345)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1356),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Bidudu',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1355)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1357),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabuye',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1355)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1358),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Karizinge',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1355)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1359),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rwagasiga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1355)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1360),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rweteto',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1355)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1365),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Uwimpunga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1364)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1396),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gisororo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1364)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1397),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabeza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1364)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1398),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Katarara',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1364)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1399),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kinihira',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1364)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1400),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rwimpyisi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1364)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1367),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ayabakiza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1366)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1368),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Bisagara',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1366)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1369),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyamigende',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1366)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1370),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rugarama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1366)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1371),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rwamakara',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1366)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1372),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Twabagarama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1366)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1374),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gikana',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1373)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1375),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gikurazo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1373)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1376),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabukuba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1373)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1377),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kamatongo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1373)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1378),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Majanja',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1373)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1379),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mbuye',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1373)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1380),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rushubi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1373)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1382),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Cyirabo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1381)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1383),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gatora',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1381)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1384),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kajevuba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1381)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1385),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mugorore',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1381)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1386),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Murambi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1381)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1387),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rebero',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1381)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1388),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rwamurama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1381)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1389),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Tabarari',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1381)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1391),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Bitega',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1390)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1392),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Cyabasonga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1390)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1393),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Cyingaju',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1390)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1394),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabeza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1390)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1395),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyaruhuru',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1390)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1403),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Akanigo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1402)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1404),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Biharagu',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1402)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1405),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kanyonyera',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1402)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1406),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Munazi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1402)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1407),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Muyigi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1402)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1408),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyarurama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1402)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1409),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rubugu',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1402)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1411),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Akabazeyi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1410)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1412),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kagenge',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1410)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1413),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Murambo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1410)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1414),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyabyondo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1410)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1415),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyakariba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1410)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1416),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rebero',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1410)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1417),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Senga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1410)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1419),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Byimana',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1418)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1420),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kampeka',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1418)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1421),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mabuye',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1418)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1422),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Masangano',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1418)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1423),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mbuganzeri',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1418)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1424),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mparo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1418)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1425),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ndama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1418)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1426),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Pamba i',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1418)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1427),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Pamba ii',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1418)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1429),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Akaje',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1428)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1430),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Fatinkanda',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1428)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1431),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Murago',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1428)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1432),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Murambi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1428)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1433),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ntungamo i',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1428)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1434),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ntungamo ii',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1428)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1435),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyakayaga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1428)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1437),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Cyogamuyaga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1436)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1438),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mububa i',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1436)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1439),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mububaya ii',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1436)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1440),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rubirizi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1436)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1441),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rusibya',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1436)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1442),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Tunda',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1436)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1443),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Twuruziramire',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1436)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1444),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Uwibiraro i',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1436)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1445),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Uwibiraro ii',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1436)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1446),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Uwumusave',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1436)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1449),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mareba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1450),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Muyange',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1451),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rukoyoyo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1452),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Runyonza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1453),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rususa',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1496),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Bigaga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1497),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Bukumba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1498),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Cyantwari',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1499),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gasagara',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1500),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gitega',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1501),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabeza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1502),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kagese',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1503),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kagogo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1504),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kamasonga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1448)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1455),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gatanga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1454)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1456),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gitwa',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1454)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1457),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabere',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1454)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1458),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kajevuba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1454)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1459),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kamudeberi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1454)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1460),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kamunana',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1454)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1461),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kanka',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1454)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1462),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kaziranyenzi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1454)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1463),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rwintare',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1454)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1465),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gafunzo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1464)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1466),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabeza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1464)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1467),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabingo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1464)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1468),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabuye',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1464)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1469),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Karwana',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1464)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1470),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ngugu',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1464)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1471),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Nyamigisha',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1464)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1472),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ruhina',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1464)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1473),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rusenyi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1464)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1474),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ruyenzi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1464)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1476),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gatare',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1475)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1477),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gatinza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1475)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1478),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gihoko',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1475)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1479),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kabuga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1475)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1480),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kagarama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1475)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1481),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Matinza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1475)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1482),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Mbuga',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1475)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1483),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rango',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1475)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1484),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rusagara',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1475)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1485),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rwabikwano',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1475)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1487),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gasagara',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1486)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1488),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gatare',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1486)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1489),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kayonza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1486)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1490),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Keza',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1486)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1491),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kururama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1486)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1492),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Muyenzi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1486)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1493),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Ruduha',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1486)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1494),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rugarama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1486)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1495),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rutaka',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1486)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1507),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gacucu',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1506)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1508),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gakamba',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1506)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1509),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gisenyi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1506)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1510),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kamugenzi',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1506)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1511),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Karambo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1506)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1512),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Kavumu',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1506)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1513),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Rukora',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1506)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1515),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Biryogo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1514)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1516),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gakindo',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1514)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1517),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Gitaramuka',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1514)))),
								_Utils_Tuple2(
								$author$project$Backend$Entities$toEntityId(1518),
								A2(
									$author$project$Utils$GeoLocation$GeoLocation,
									'Karama',
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(1514))))
							]),
						_Utils_ap(
							_List_fromArray(
								[
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1519),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kiruhura',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1514)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1520),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Remera',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1514)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1521),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rukindo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1514)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1522),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Taba',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1514)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1523),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Tetero',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1514)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1525),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gahwiji i',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1524)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1526),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gahwiji ii',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1524)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1527),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kindonyi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1524)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1528),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Murambi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1524)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1529),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Ruhorobero',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1524)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1530),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwakaramira',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1524)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1531),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwarusaku',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1524)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1533),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gacyamo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1532)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1534),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gahinga',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1532)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1535),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gisenyi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1532)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1536),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gitera',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1532)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1537),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kibirizi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1532)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1538),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugazi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1532)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1539),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwakibirizi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1532)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1541),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Cyaruhiririra',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1540)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1542),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kabyo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1540)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1543),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugarama',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1540)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1544),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwimikoni i',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1540)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1545),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwimikoni ii',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1540)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1548),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Muhanga',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1549),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nunga',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1550),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyagasagara',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1551),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugando',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1552),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugeyo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1588),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Bidudu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1589),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Bishinge',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1590),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Bizenga',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1591),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Cyeru',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1592),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gakomeye',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1593),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gakurazo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1594),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kigarama',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1595),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kijuri',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1596),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kiringa',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1547)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1554),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatoki',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1553)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1555),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gitagata',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1553)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1556),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kigusa',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1553)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1557),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kiruhura',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1553)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1558),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Mbonwa',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1553)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1559),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyakajuri',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1553)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1560),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugarama',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1553)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1561),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rushubi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1553)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1562),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwankeri',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1553)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1564),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kabeza',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1563)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1565),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kabuye',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1563)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1566),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kagunga',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1563)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1567),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kanyamata',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1563)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1568),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Karambo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1563)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1569),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Karubanzangabo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1563)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1570),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kinyovi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1563)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1571),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyamuri',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1563)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1572),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rulindo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1563)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1573),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Runyonza',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1563)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1575),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Bidudu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1576),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Cyanika',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1577),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Cyarubazi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1578),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatare',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1579),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gihari',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1580),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kagusa',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1581),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kamahango',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1582),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kavumu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1583),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kidudu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1584),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Migina',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1585),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Ngarama',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1586),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Remera',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1587),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rusagara',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1574)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1599),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Bitaba',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1598)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1600),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatwe',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1598)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1601),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gisasa',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1598)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1602),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Misatsi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1598)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1603),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rebero',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1598)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1604),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rukoronko',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1598)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1606),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatare',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1605)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1607),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gisenyi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1605)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1608),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Karutabana',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1605)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1609),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Ngando',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1605)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1610),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rubumba',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1605)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1611),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwintenderi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1605)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1613),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kagerero',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1612)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1614),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyamabuye',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1612)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1615),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyarukombe',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1612)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1616),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugazi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1612)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1617),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rukira',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1612)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1618),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rukore',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1612)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1619),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rusagara',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1612)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1621),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatoki',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1620)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1622),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gitaraga',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1620)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1623),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kaboshya',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1620)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1624),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kaziramire',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1620)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1625),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rurenge',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1620)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1626),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwabashenyi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1620)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1629),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kamugera',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1628)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1632),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kamugore',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1628)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1633),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kigarama',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1628)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1635),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rubirizi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1628)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1638),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugazi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1628)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1640),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Runyonza',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1628)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1641),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rusibya',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1628)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1687),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Akajuri',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1628)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1688),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kabare',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1628)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1689),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kabumbwe',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1628)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1690),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kagano',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1628)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1631),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kabaya',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1634),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kabuye',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1636),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kadebu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1637),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kagasa',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1639),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Karambo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1642),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kirasaniro',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1643),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kururama',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1644),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyakariba',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1645),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyarubande',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1646),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rusagara',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1647),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rutare',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1648),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Ruzinge',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1649),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Shitwe',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1650),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Buhara',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1630)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1652),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Agashyamba',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1653),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Bishenyi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1654),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Fatinkanda',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1655),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gakurazo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1656),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatanga',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1657),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Ikoni',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1658),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kagege',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1659),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kankuriyingoma',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1660),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kigandu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1661),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kinamba',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1662),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Murama',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1663),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Muyange',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1664),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyakagarama',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1665),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rusamaza',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1666),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwabisheshe',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1667),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Shami',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1651)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1669),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Binyonzwe',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1668)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1670),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kamajeri',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1668)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1671),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kamasonga',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1668)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1672),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Karugondo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1668)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1673),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kivugiza',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1668)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1674),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Muyange',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1668)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1675),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Ngeruka',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1668)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1677),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Heru',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1676)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1678),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kabuye',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1676)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1679),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Karama',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1676)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1680),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kavumu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1676)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1681),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kibaya',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1676)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1682),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kibungo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1676)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1683),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kimiduha',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1676)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1684),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Murambi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1676)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1685),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyakayenzi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1676)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1686),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Twimpara',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1676)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1693),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatoro',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1692)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1694),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kayenzi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1692)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1695),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kidudu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1692)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1696),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kingabo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1692)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1697),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rubomborana',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1692)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1698),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugarama',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1692)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1699),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugunga',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1692)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1701),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Cyeru',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1700)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1702),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gasagara',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1700)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1703),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kabaha',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1700)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1704),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kabeza',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1700)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1705),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Karumuna',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1700)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1706),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kurugenge',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1700)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1707),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyamabuye',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1700)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1708),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwangara',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1700)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1710),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kagoma i',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1709)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1711),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kagoma ii',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1709)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1712),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kiganwa',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1709)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1713),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nganwa',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1709)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1714),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyarunazi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1709)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1715),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Ruhengeri',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1709)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1716),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rusekera',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1709)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1719),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugarama',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1718)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1720),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rusagara',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1718)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1766),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gahembe',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1718)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1767),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gisunzu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1718)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1768),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Mukoma',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1718)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1769),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Muyange',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1718)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1722),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Bishweshwe',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1721)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1723),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gataraga',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1721)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1724),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatare',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1721)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1725),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kasebigege',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1721)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1726),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kivugiza',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1721)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1727),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kiyogoma',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1721)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1728),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Mwesa',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1721)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1729),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rucucu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1721)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1730),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Ruhanga',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1721)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1731),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rutobotobo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1721)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1732),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rutukura',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1721)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1734),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gasenga i',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1735),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gasenga ii',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1736),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatare i',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1737),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatare ii',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1738),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatare iii',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1739),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyabivumu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1740),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyamata i',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1741),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyamata ii',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1742),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugarama i',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1743),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugarama ii',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1744),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugarama iii',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1745),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwakibirizi i',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1746),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwakibirizi ii',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1733)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1748),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Bihari',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1747)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1749),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Cyeru',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1747)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1750),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gitovu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1747)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1751),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kagirazina',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1747)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1752),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Musagara',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1747)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1753),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyarugati i',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1747)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1754),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyarugati ii',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1747)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1755),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugando',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1747)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1756),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Sumbure',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1747)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1758),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatare',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1757)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1759),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Karambi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1757)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1760),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kayenzi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1757)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1761),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Murambi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1757)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1762),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyagatovu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1757)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1763),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyakwibereka',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1757)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1764),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyiramatuntu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1757)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1765),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwanza',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1757)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1772),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kamabare',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1771)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1773),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kamugera',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1771)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1774),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kiyovu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1771)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1775),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Muyange',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1771)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1776),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyagisenyi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1771)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1777),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rubona',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1771)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1778),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugasa',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1771)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1779),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwashangwe',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1771)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1780),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Tubumba',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1771)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1814),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Bushonyi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1771)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1782),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Bushenyi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1781)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1783),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gako',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1781)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1784),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kamahirwe',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1781)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1785),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nsoro',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1781)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1786),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rebero',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1781)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1787),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rugero',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1781)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1789),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Mabanga',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1788)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1790),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Mwoshya',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1788)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1791),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Ntungamo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1788)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1792),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyabuhoro',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1788)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1793),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyagasozi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1788)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1794),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyarubande',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1788)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1795),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Rwabusoro',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1788)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1797),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Cyahafi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1796)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1798),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gateko',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1796)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1799),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Gatoki',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1796)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1800),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Karubagazi',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1796)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1801),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyakabingo',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1796)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1802),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyakabuye',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1796)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1803),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Nyarusambu',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1796)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1805),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Cundaminega',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1804)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1806),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Cyeru',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1804)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1807),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kadogori',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1804)))),
									_Utils_Tuple2(
									$author$project$Backend$Entities$toEntityId(1808),
									A2(
										$author$project$Utils$GeoLocation$GeoLocation,
										'Kanombe',
										$elm$core$Maybe$Just(
											$author$project$Backend$Entities$toEntityId(1804))))
								]),
							_Utils_ap(
								_List_fromArray(
									[
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1809),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kayitanga',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1804)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1810),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyagakombe',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1804)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1811),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rugandara',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1804)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1812),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rurama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1804)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1813),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rushorezo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1804)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1817),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gaseke',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1816)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1818),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gasenyi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1816)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1827),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gitovu',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1816)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1844),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kagugu',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1816)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1847),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kamashya',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1816)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1852),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kavumu',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1816)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1855),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ntarama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1816)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1858),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyamure',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1816)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1860),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rurambo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1816)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1862),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akabeza',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1816)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1863),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Saruduha',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1816)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1864),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gasave',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1816)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1820),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Bidenge',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1821),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Biraro',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1822),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Bwiza',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1823),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gako',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1824),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gasarwe',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1825),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gasave',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1826),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gitega',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1828),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kabeza',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1829),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kagarama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1830),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Karambi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1831),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Karambo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1832),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Karirisi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1833),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Marembo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1834),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyamisagara',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1819)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1836),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gakurazo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1835)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1837),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gatare',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1835)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1838),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kamahoro',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1835)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1839),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mutarama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1835)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1840),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruyenzi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1835)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1841),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rwankomati',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1835)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1842),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rwavuningoma',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1835)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1843),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rwimirama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1835)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1846),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akintwari',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1845)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1848),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akumunezero',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1845)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1849),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Amizero',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1845)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1850),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Buhoro',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1845)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1851),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Byimana',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1845)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1853),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gasabo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1845)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1854),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gihushi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1845)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1856),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akabahaya',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1845)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1857),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kidogo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1845)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1859),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kimaranzara',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1845)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1861),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kivumu',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1845)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1866),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Cyoma',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1865)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1867),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gicaca',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1865)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1868),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kamabuye',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1865)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1869),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Karama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1865)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1870),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mataba',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1865)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1871),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mubuga',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1865)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1872),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mukoma',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1865)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1873),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Murambi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1865)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1874),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyabagendwa',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1865)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1875),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyamizi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1865)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1876),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rwibikara',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1865)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1879),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Saruduha',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1878)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1909),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gatare',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1878)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1910),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gatovu',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1878)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1911),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kagasera',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1878)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1912),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kamweru',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1878)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1913),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kibaza',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1878)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1914),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kindama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1878)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1915),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rebero',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1878)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1916),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruramba',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1878)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1917),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rutare',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1878)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1881),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kimikamba',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1880)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1882),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mubano',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1880)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1883),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyabaranga',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1880)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1884),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruhuha i',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1880)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1885),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruhuha ii',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1880)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1887),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Bihari',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1886)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1888),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Busasamana',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1886)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1889),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Masenga i',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1886)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1890),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Masenga ii',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1886)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1891),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mukoma',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1886)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1892),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyagafunzo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1886)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1893),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rugarama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1886)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1894),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rwanzunga',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1886)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1896),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Butereri',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1895)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1897),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kayigi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1895)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1898),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kibaza',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1895)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1899),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyaburiba',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1895)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1900),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyakagarama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1895)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1901),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rwanika',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1895)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1903),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gikundamvura',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1902)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1904),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kanombe',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1902)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1905),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kazabagarura',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1902)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1906),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kiyovu',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1902)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1907),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rukurazo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1902)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1908),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rusenyi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1902)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1920),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Agahonnyo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1919)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1921),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Batima',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1919)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1922),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gasororo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1919)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1923),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gikoma',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1919)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1924),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ihara',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1919)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1925),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kamudusi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1919)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1926),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mbuganzeri',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1919)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1927),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rubira',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1919)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1928),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruhehe',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1919)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1929),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Twinyange',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1919)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1931),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gakindo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1930)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1932),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gasenyi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1930)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1933),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Maburane',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1930)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1934),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mugina',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1930)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1935),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyiragiseke',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1930)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1936),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyirakanemba',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1930)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1937),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyirarubomboza',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1930)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1938),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nzangwa',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1930)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1939),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ubukoroco',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1930)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1941),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gasasa',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1940)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1942),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rukira',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1940)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1943),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rusenyi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1940)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1945),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kigina',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1944)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1946),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kimpara',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1944)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1947),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kimvubu',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1944)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1948),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Muyoboro',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1944)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1949),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nemba',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1944)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1950),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyakabingo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1944)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1951),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rutete',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1944)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1952),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rwibinyogote',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1944)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1953),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rwiminazi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1944)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1955),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Agashoro',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1954)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1956),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kivusha',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1954)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1957),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mujwiri',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1954)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1958),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mushyoroti',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1954)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1959),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nkanga',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1954)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1960),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruzo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1954)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1962),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Karizinge',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1961)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1963),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Sharita',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1961)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1966),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kabagugu',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1965)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1967),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kinteko',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1965)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1968),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ngaruye',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1965)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1969),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rwamanyoni',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1965)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1971),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gakoni',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1970)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1972),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyabaguma',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1970)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1973),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rubwirwa',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1970)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1975),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gahosha',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1974)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1976),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kagarama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1974)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1977),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nziranziza',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1974)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1978),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruli',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1974)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1980),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gateko',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1979)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1981),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyamirama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1979)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1982),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rebero',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1979)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1983),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rutebe',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1979)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1985),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gaseke',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1984)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1986),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kamweru',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1984)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1987),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruhanga',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1984)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1988),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rutare',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1984)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1989),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Shyara',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1984)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1994),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyabugogo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1993)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1995),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruhondo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1993)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2038),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gakoni',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1993)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2039),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gatare',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1993)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2040),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Giticyinyoni',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1993)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2041),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kadobogo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1993)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2042),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kamenge',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1993)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2043),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Karama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1993)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2044),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kiruhura',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1993)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2045),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyabikoni',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1993)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1997),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Misibya',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1996)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1998),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyabitare',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1996)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(1999),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruhango',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1996)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2000),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruharabuge',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1996)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2001),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruriba',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1996)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2002),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruzigimbogo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1996)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2003),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ryamakomari',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1996)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2004),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Tubungo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(1996)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2006),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akanyamirambo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2005)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2009),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akinama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2005)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2011),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Makaga',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2005)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2013),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Musimba',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2005)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2015),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruhogo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2005)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2016),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rwesero',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2005)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2018),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rweza',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2005)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2020),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Vuganyana',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2005)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2008),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akirwanda',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2007)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2010),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gisenga',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2007)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2012),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kadobogo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2007)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2014),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kagarama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2007)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2017),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kibisogi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2007)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2019),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Muganza',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2007)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2021),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Murama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2007)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2022),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rubuye',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2007)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2023),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruhango',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2007)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2024),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ryasharangabo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2007)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2026),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Agakomeye',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2025)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2027),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akagugu',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2025)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2028),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Amahoro',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2025)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2029),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Amajyambere',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2025)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2030),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Birambo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2025)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2031),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Isangano',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2025)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2032),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kanyabami',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2025)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2033),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Karambo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2025)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2034),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mwendo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2025)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2035),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruhuha',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2025)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2036),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ubuzima',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2025)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2037),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Umutekano',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2025)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2049),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akakaza',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2048)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2050),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Muhozi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2048)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2051),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rubungo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2048)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2052),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ryakigogo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2048)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2053),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Zindiro',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2048)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2055),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kagarama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2054)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2056),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kayumba',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2054)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2057),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ramba',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2054)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2058),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rebero',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2054)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2059),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rugando',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2054)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2061),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kigabiro',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2060)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2062),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kiyoro',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2060)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2063),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Murarambo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2060)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2064),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nkona',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2060)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2065),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyakabingo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2060)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2066),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rukoma',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2060)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2068),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Birembo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2067)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2069),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gisasa',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2067)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2070),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Munini',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2067)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2071),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Ruhinga',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2067)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2072),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Uwaruraza',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2067)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2074),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akabenejuru',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2073)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2075),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akasedogo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2073)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2076),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akimpama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2073)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2077),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Burima',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2073)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2078),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kityazo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2073)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2080),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Bushya',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2079)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2081),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gikumba',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2079)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2082),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kamatamu',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2079)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2083),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Karama',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2079)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2084),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kayenzi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2079)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2085),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kigara',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2079)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2086),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kiriza',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2079)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2087),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Masizi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2079)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2088),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mbogo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2079)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2089),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyampamo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2079)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2091),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akanyiramugarura',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2090)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2092),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akigabiro',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2090)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2093),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Gishaka',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2090)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2094),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kabuye',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2090)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2095),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Mpabwa',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2090)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2096),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyagasambu',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2090)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2097),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Urutarishonga',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2090)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2100),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akamamana',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2099)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2101),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Akimihigo',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2099)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2102),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Bigega',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2099)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2103),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Busasamana',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2099)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2104),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kingasire',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2099)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2105),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Kumuyange',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2099)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2106),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Muremera',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2099)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2107),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Nyagasozi',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2099)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2108),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rugoro',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2099)))),
										_Utils_Tuple2(
										$author$project$Backend$Entities$toEntityId(2109),
										A2(
											$author$project$Utils$GeoLocation$GeoLocation,
											'Rwesero',
											$elm$core$Maybe$Just(
												$author$project$Backend$Entities$toEntityId(2099))))
									]),
								_Utils_ap(
									_List_fromArray(
										[
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2110),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Tetero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2099)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2112),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agakomeye',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2113),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gashubi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2114),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gisiza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2115),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Hanika',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2116),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Juru',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2117),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kibaya',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2118),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mpakabavu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2119),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Musango',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2120),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ndengo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2121),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyakabande',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2122),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyakanunga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2123),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rubonobono',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2124),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Runyonza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2125),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rusoro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2126),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ruvumero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2127),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Uwagatovu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2111)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2129),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agataramo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2128)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2130),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akamwunguzi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2128)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2131),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akarubimbura',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2128)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2132),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akisoko',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2128)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2133),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amarembo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2128)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2134),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amizero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2128)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2135),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bwiza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2128)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2136),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ihuriro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2128)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2137),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Isangano',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2128)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2138),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kanyonyomba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2128)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2139),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyakariba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2128)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2140),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rwakarihejuru',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2128)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2143),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2142)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2144),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Twina',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2142)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2165),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kimisebeya',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2142)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2166),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kivugiza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2142)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2146),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bwimiyange',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2145)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2147),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bwingeyo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2145)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2148),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasagara',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2145)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2149),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugwiza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2145)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2151),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ntaganzwa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2150)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2152),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyagasozi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2150)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2153),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyagisozi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2150)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2154),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ruganda',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2150)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2156),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gahinga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2155)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2157),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasharu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2155)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2158),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kibobo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2155)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2159),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nombe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2155)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2161),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Munini',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2160)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2162),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mutokerezwa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2160)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2163),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rudakabukirwa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2160)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2164),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Runyinya',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2160)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2169),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amajyambere',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2168)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2170),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amarembo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2168)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2171),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Byimana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2168)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2172),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasave',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2168)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2173),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasharu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2168)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2174),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kagara',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2168)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2175),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyakariba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2168)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2176),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rwinyana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2168)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2178),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kanyinya',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2177)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2179),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kumukenke',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2177)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2180),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Murambi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2177)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2181),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ntora',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2177)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2182),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rukeri',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2177)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2183),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Umurava',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2177)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2186),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akamatamu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2185)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2187),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Cyeyere',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2185)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2188),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Murehe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2185)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2189),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyacyonga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2185)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2192),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyagasozi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2185)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2194),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyarukurazo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2185)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2191),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akabuga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2190)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2193),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Jurwe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2190)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2199),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kiberinka',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2190)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2201),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyakirehe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2190)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2203),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyarubuye',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2190)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2205),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rubona',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2190)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2207),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rwanyanza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2190)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2209),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Uwanyange',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2190)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2230),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agahama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2190)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2231),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agasharu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2190)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2196),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agakenke',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2195)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2197),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agatare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2195)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2198),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akinyana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2195)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2200),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gikingo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2195)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2202),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gitega',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2195)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2204),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gitenga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2195)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2206),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyakabingo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2195)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2208),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyarurama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2195)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2210),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugogwe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2195)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2211),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Taba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2195)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2213),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amakawa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2212)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2214),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amasangano',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2212)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2215),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Buriza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2212)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2216),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ihuriro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2212)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2217),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabeza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2212)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2218),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Karuruma',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2212)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2219),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Murama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2212)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2220),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyagasozi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2212)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2221),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rebero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2212)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2222),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2212)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2223),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Tetero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2212)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2225),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agasekabuye',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2224)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2226),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agatare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2224)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2227),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amasangano',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2224)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2228),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mubuga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2224)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2229),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyamweru',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2224)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2234),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bugarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2233)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2235),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bukamba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2233)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2236),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Byimana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2233)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2237),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabizoza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2233)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2238),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kinunga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2233)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2239),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Urunyinya',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2233)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2240),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rwankuba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2233)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2242),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabande',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2241)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2243),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gatare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2241)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2244),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyamugali',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2241)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2245),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyarubuye',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2241)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2247),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gahinga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2246)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2248),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gatare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2246)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2249),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Umunyinya',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2246)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2251),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agatwa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2250)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2252),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabagina',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2250)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2253),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kajevuba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2250)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2254),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kigarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2250)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2255),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyagasayo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2250)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2257),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyaburira',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2256)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2258),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kirehe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2256)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2259),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mataba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2256)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2260),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyarurembo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2256)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2261),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rubona',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2256)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2263),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bwocya',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2262)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2264),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gitaba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2262)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2265),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Karenge',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2262)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2266),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugina',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2262)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2267),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ruhihi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2262)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2269),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agasharu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2268)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2270),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agatare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2268)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2271),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabuga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2268)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2272),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Runyinya',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2268)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2275),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rwinzovu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2274)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2276),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Urugwiro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2274)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2277),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Uruhongore',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2274)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2300),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amajyambere',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2274)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2301),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bukinanyana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2274)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2302),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Cyimana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2274)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2303),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gataba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2274)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2304),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Itetero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2274)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2305),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2274)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2306),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kamuhire',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2274)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2307),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Karukamba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2274)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2308),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyagacyamo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2274)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2279),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agasaro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2278)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2280),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasharu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2278)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2281),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Inkingi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2278)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2282),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kanserege',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2278)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2283),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kigugu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2278)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2284),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ruganwa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2278)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2285),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Umuco',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2278)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2286),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Urugero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2278)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2287),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Urwibutso',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2278)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2289),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amahoro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2288)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2290),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bwiza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2288)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2291),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ihuriro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2288)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2292),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ineza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2288)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2293),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Inyange',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2288)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2294),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Iriba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2288)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2295),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabagari',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2288)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2296),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ubumwe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2288)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2297),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Umutako',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2288)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2298),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Urukundo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2288)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2299),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Virunga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2288)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2311),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Inyamibwa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2310)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2312),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Isangano',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2310)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2313),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Isano',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2310)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2314),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ituze',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2310)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2315),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Izuba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2310)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2316),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Juru',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2310)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2317),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyenyeri',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2310)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2318),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Umurava',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2310)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2319),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Urumuri',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2310)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2321),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amahoro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2320)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2322),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amajyambere',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2320)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2323),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Imihigo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2320)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2324),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Intambwe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2320)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2325),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mutara',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2320)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2326),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2320)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2327),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ubumwe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2320)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2328),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Umutekano',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2320)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2329),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Urwego',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2320)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2331),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasange',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2330)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2332),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasasa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2330)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2333),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rebero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2330)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2334),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Taba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2330)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2337),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Inganji',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2338),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ingenzi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2339),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ingeri',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2340),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Inshuti',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2341),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Intashyo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2342),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Intwari',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2343),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Inyamibwa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2344),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Inyange',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2345),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ubwiza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2346),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Umwezi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2369),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Abatuje',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2370),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amariza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2371),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Imanzi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2372),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Imena',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2373),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Imitari',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2336)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2348),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akintwari',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2349),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Buranga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2350),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasharu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2351),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ibuhoro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2352),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kageyo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2353),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kamahinda',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2354),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Karisimbi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2355),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Karongi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2356),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyirabwana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2357),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ramiro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2358),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rindiro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2359),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2360),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rukurazo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2361),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Urumuri',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2347)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2363),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ibukinanyana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2362)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2364),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ibuhoro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2362)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2365),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ijabiro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2362)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2366),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Isangano',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2362)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2367),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Itetero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2362)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2368),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Urugwiro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2362)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2376),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ngaruyinka',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2375)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2377),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rusenyi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2375)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2378),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Taba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2375)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2401),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Binunga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2375)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2380),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akarambo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2379)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2381),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akaruvusha',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2379)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2382),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Estate 2020',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2379)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2383),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabuhunde ii',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2379)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2384),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Urugarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2379)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2385),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ururembo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2379)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2402),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Umucyo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2379)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2387),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agatare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2386)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2388),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasharu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2386)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2389),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kami',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2386)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2390),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rwankuba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2386)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2392),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Dusenyi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2391)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2393),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gicikiza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2391)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2394),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Giheka',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2391)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2395),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabuhunde i',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2391)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2396),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kadobogo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2391)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2397),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kagarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2391)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2398),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Muhororo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2391)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2399),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyakabungo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2391)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2400),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rukingu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2391)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2405),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akarwasa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2404)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2406),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akasemuromba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2404)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2407),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bucyemba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2404)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2408),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasharu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2404)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2409),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mukagarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2404))))
										]),
									_List_fromArray(
										[
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2410),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ruhangare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2404)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2412),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ayabakora',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2411)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2413),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Cyaruzinge',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2411)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2414),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gashure',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2411)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2415),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gatare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2411)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2416),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gisura',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2411)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2417),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Karubibi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2411)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2418),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mulindi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2411)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2420),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bahoze',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2419)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2421),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Berwa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2419)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2422),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Buhoro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2419)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2423),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Burunga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2419)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2424),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gitaraga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2419)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2425),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kira',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2419)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2426),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nezerwa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2419)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2427),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugazi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2419)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2428),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Runyonza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2419)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2429),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Tumurere',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2419)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2430),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ururembo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2419)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2432),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Byimana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2431)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2433),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabeza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2431)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2434),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Masoro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2431)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2435),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Matwari',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2431)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2436),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mubuga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2431)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2437),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Munini',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2431)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2439),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akamusare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2438)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2440),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akimana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2438)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2441),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasharu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2438)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2442),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Jurwe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2438)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2443),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Karambo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2438)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2444),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kigabiro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2438)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2445),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ruseno',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2438)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2447),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kacyinyaga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2446)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2448),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kamahoro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2446)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2449),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Munini',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2446)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2450),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyakagezi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2446)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2451),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ruhangare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2446)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2452),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ruhogo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2446)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2455),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyarubuye',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2454)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2456),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyura',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2454)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2502),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kanani',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2454)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2503),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kidahe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2454)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2504),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kigabiro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2454)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2505),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyamurambi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2454)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2458),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gatagara',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2457)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2459),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kagarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2457)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2460),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyabitare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2457)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2461),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyakabungo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2457)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2462),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyarubande',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2457)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2463),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Uruhetse',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2457)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2465),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agacyamo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2464)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2466),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gashinya',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2464)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2467),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gikombe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2464)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2468),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kazi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2464)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2469),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kigufi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2464)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2470),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyirakibehe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2464)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2471),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Uruhahiro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2464)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2473),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agasharu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2472)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2474),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amataba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2472)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2475),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Burungero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2472)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2476),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Karama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2472)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2477),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyange',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2472)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2478),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rebero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2472)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2479),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Uruyange',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2472)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2481),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gatobotobo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2480)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2482),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kibungo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2480)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2483),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Musezero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2480)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2484),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyaburoro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2480)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2485),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Taba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2480)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2487),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bikumba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2486)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2488),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gakizi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2486)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2489),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gatare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2486)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2490),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kamuyange',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2486)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2491),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kigarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2486)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2492),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ngara',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2486)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2494),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Akazi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2493)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2495),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kaduha',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2493)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2496),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kamuhoza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2493)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2497),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mirambi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2493)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2498),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Munini',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2493)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2499),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ndanyoye',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2493)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2500),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyamigina',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2493)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2501),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2493)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2508),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kamahwa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2507)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2509),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kangondo i',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2507)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2510),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kangondo ii',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2507)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2511),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kibiraro i',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2507)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2512),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kibiraro ii',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2507)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2535),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gishushu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2507)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2536),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Juru',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2507)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2514),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agashyitsi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2513)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2515),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amajyambere',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2513)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2516),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Izuba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2513)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2517),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gisimenti',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2513)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2518),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ubumwe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2513)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2519),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ukwezi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2513)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2520),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Urumuri',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2513)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2522),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amahoro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2521)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2523),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rebero',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2521)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2524),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ruturusu i',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2521)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2525),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ruturusu ii',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2521)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2526),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ubumwe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2521)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2528),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amarembo i',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2527)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2529),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amarembo il',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2527)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2530),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gihogere',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2527)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2531),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kagara',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2527)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2532),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kinunga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2527)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2533),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyabisindu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2527)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2534),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2527)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2539),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kalisimbi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2538)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2551),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Abatangampundu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2538)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2552),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Amahoro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2538)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2553),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Isangano',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2538)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2554),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabeza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2538)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2555),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Masango',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2538)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2541),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bisenga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2540)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2542),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gakenyeri',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2540)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2543),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasiza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2540)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2544),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kidogo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2540)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2546),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Agatare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2545)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2547),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasagara',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2545)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2548),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kamasasa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2545)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2549),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugagi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2545)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2550),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ryabazana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2545)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2557),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Bwiza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2556)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2558),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Cyanamo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2556)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2559),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gatare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2556)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2560),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kamashashi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2556)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2561),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mataba',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2556)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2562),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyagakombe',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2556)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2563),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ruhangare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2556)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2565),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Busenyi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2564)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2566),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kigabiro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2564)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2567),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kinyana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2564)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2568),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyagisozi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2564)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2570),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Cyeru',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2569)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2571),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Karambo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2569)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2572),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kataruha',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2569)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2573),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mugeyo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2569)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2574),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2569)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2575),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Samuduha',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2569)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2577),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gisharara',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2576)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2578),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabutare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2576)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2579),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kanyinya',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2576)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2580),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kigarama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2576)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2581),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyarucundura',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2576)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2582),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Runyonza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2576)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2583),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Urumuri',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2576)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2585),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kinyaga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2584)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2586),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mirama',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2584)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2587),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyagacyamo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2584)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2588),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rugende',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2584)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2589),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Ruhanga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2584)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2592),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Gasharu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2591)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2593),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Mulindi',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2591)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2594),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Vugavuge',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2591)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2596),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabarera',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2595)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2597),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kamusengo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2595)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2598),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Karekare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2595)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2599),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Karuranga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2595)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2600),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyakabande',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2595)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2602),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kabaliza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2601)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2603),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyamise',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2601)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2604),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rwanyanza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2601)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2606),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Cyili',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2605)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2607),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kacyatwa',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2605)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2608),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kandamira',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2605)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2609),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kantabana',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2605)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2610),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Munini',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2605)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2612),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Abanyangeyo',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2611)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2613),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kibenga',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2611)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2614),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Nyamvumvu',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2611)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2616),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kamusare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2615)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2617),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Karwiru',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2615)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2618),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Kigabiro',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2615)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2619),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rukerereza',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2615)))),
											_Utils_Tuple2(
											$author$project$Backend$Entities$toEntityId(2620),
											A2(
												$author$project$Utils$GeoLocation$GeoLocation,
												'Rwintare',
												$elm$core$Maybe$Just(
													$author$project$Backend$Entities$toEntityId(2615))))
										]))))))))));
var $author$project$Utils$GeoLocation$geoInfo = {cells: $author$project$Utils$GeoLocation$getGeoCells, districts: $author$project$Utils$GeoLocation$getGeoDistricts, provinces: $author$project$Utils$GeoLocation$getGeoProvinces, sectors: $author$project$Utils$GeoLocation$getGeoSectors, villages: $author$project$Utils$GeoLocation$getGeoVillages};
var $pzp1997$assoc_list$AssocList$toList = function (_v0) {
	var alist = _v0.a;
	return alist;
};
var $author$project$Utils$GeoLocation$geoLocationDictToOptions = A2(
	$elm$core$Basics$composeR,
	$pzp1997$assoc_list$AssocList$toList,
	$elm$core$List$map(
		function (_v0) {
			var id = _v0.a;
			var geoLocation = _v0.b;
			return _Utils_Tuple2(
				$elm$core$String$fromInt(
					$author$project$Backend$Entities$fromEntityId(id)),
				geoLocation.name);
		}));
var $pzp1997$assoc_list$AssocList$get = F2(
	function (targetKey, _v0) {
		get:
		while (true) {
			var alist = _v0.a;
			if (!alist.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var _v2 = alist.a;
				var key = _v2.a;
				var value = _v2.b;
				var rest = alist.b;
				if (_Utils_eq(key, targetKey)) {
					return $elm$core$Maybe$Just(value);
				} else {
					var $temp$targetKey = targetKey,
						$temp$_v0 = $pzp1997$assoc_list$AssocList$D(rest);
					targetKey = $temp$targetKey;
					_v0 = $temp$_v0;
					continue get;
				}
			}
		}
	});
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm_community$maybe_extra$Maybe$Extra$isJust = function (m) {
	if (m.$ === 'Nothing') {
		return false;
	} else {
		return true;
	}
};
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Pages$Utils$emptySelectOption = function (isSelected) {
	return A2(
		$elm$html$Html$option,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$value(''),
				$elm$html$Html$Attributes$selected(isSelected)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text('')
			]));
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$select = _VirtualDom_node('select');
var $author$project$Pages$Utils$viewCustomLabel = F4(
	function (language, translationId, suffix, class_) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(class_)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(
					_Utils_ap(
						A2($author$project$Translate$translate, language, translationId),
						suffix))
				]));
	});
var $author$project$Pages$Utils$viewLabel = F2(
	function (language, translationId) {
		return A4($author$project$Pages$Utils$viewCustomLabel, language, translationId, ':', 'label');
	});
var $author$project$Pages$Menu$View$viewSelectListInput = F6(
	function (language, currentValue, options, setMsg, labelTransId, disabled) {
		var emptyOption = $author$project$Pages$Utils$emptySelectOption(
			_Utils_eq(currentValue, $elm$core$Maybe$Nothing));
		var selectOptions = A2(
			$elm$core$List$cons,
			emptyOption,
			A2(
				$elm$core$List$map,
				function (option_) {
					var isSelected = A2(
						$elm$core$Maybe$withDefault,
						false,
						A2(
							$elm$core$Maybe$map,
							function (id) {
								return _Utils_eq(
									currentValue,
									$elm$core$Maybe$Just(
										$author$project$Backend$Entities$toEntityId(id)));
							},
							$elm$core$String$toInt(option_.a)));
					return A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value(option_.a),
								$elm$html$Html$Attributes$selected(isSelected)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(option_.b)
							]));
				},
				options));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('select-input-wrapper', true),
							_Utils_Tuple2('disabled', disabled)
						]))
				]),
			_List_fromArray(
				[
					A2($author$project$Pages$Utils$viewLabel, language, labelTransId),
					A2(
					$elm$html$Html$select,
					_List_fromArray(
						[
							$elm$html$Html$Events$onInput(setMsg),
							$elm$html$Html$Attributes$class('select-input')
						]),
					selectOptions)
				]));
	});
var $author$project$Pages$Menu$View$view = F2(
	function (language, model) {
		var villageInput = A2(
			$elm$core$Maybe$withDefault,
			$author$project$Gizra$Html$emptyNode,
			A2(
				$elm$core$Maybe$map,
				function (parentId) {
					var options = $author$project$Utils$GeoLocation$geoLocationDictToOptions(
						A2(
							$author$project$Utils$GeoLocation$filterGeoLocationDictByParent,
							$author$project$Backend$Entities$fromEntityId(parentId),
							$author$project$Utils$GeoLocation$geoInfo.villages));
					return A6(
						$author$project$Pages$Menu$View$viewSelectListInput,
						language,
						model.village,
						options,
						$author$project$Pages$Menu$Model$SetGeoLocation(
							F2(
								function (value, form) {
									return _Utils_update(
										form,
										{
											village: A2(
												$elm$core$Maybe$map,
												$author$project$Backend$Entities$toEntityId,
												$elm$core$String$toInt(value))
										});
								})),
						$author$project$Translate$Village,
						false);
				},
				model.cell));
		var sectorInput = A2(
			$elm$core$Maybe$withDefault,
			$author$project$Gizra$Html$emptyNode,
			A2(
				$elm$core$Maybe$map,
				function (parentId) {
					var options = $author$project$Utils$GeoLocation$geoLocationDictToOptions(
						A2(
							$author$project$Utils$GeoLocation$filterGeoLocationDictByParent,
							$author$project$Backend$Entities$fromEntityId(parentId),
							$author$project$Utils$GeoLocation$geoInfo.sectors));
					return A6(
						$author$project$Pages$Menu$View$viewSelectListInput,
						language,
						model.sector,
						options,
						$author$project$Pages$Menu$Model$SetGeoLocation(
							F2(
								function (value, form) {
									return _Utils_update(
										form,
										{
											sector: A2(
												$elm$core$Maybe$map,
												$author$project$Backend$Entities$toEntityId,
												$elm$core$String$toInt(value))
										});
								})),
						$author$project$Translate$Sector,
						$elm_community$maybe_extra$Maybe$Extra$isJust(model.cell));
				},
				model.district));
		var provinceInput = function () {
			var options = $author$project$Utils$GeoLocation$geoLocationDictToOptions($author$project$Utils$GeoLocation$geoInfo.provinces);
			return A6(
				$author$project$Pages$Menu$View$viewSelectListInput,
				language,
				model.province,
				options,
				$author$project$Pages$Menu$Model$SetGeoLocation(
					F2(
						function (value, form) {
							return _Utils_update(
								form,
								{
									province: A2(
										$elm$core$Maybe$map,
										$author$project$Backend$Entities$toEntityId,
										$elm$core$String$toInt(value))
								});
						})),
				$author$project$Translate$Province,
				$elm_community$maybe_extra$Maybe$Extra$isJust(model.district));
		}();
		var districtInput = A2(
			$elm$core$Maybe$withDefault,
			$author$project$Gizra$Html$emptyNode,
			A2(
				$elm$core$Maybe$map,
				function (parentId) {
					var options = $author$project$Utils$GeoLocation$geoLocationDictToOptions(
						A2(
							$author$project$Utils$GeoLocation$filterGeoLocationDictByParent,
							$author$project$Backend$Entities$fromEntityId(parentId),
							$author$project$Utils$GeoLocation$geoInfo.districts));
					return A6(
						$author$project$Pages$Menu$View$viewSelectListInput,
						language,
						model.district,
						options,
						$author$project$Pages$Menu$Model$SetGeoLocation(
							F2(
								function (value, form) {
									return _Utils_update(
										form,
										{
											district: A2(
												$elm$core$Maybe$map,
												$author$project$Backend$Entities$toEntityId,
												$elm$core$String$toInt(value))
										});
								})),
						$author$project$Translate$District,
						$elm_community$maybe_extra$Maybe$Extra$isJust(model.sector));
				},
				model.province));
		var cellInput = A2(
			$elm$core$Maybe$withDefault,
			$author$project$Gizra$Html$emptyNode,
			A2(
				$elm$core$Maybe$map,
				function (parentId) {
					var options = $author$project$Utils$GeoLocation$geoLocationDictToOptions(
						A2(
							$author$project$Utils$GeoLocation$filterGeoLocationDictByParent,
							$author$project$Backend$Entities$fromEntityId(parentId),
							$author$project$Utils$GeoLocation$geoInfo.cells));
					return A6(
						$author$project$Pages$Menu$View$viewSelectListInput,
						language,
						model.cell,
						options,
						$author$project$Pages$Menu$Model$SetGeoLocation(
							F2(
								function (value, form) {
									return _Utils_update(
										form,
										{
											cell: A2(
												$elm$core$Maybe$map,
												$author$project$Backend$Entities$toEntityId,
												$elm$core$String$toInt(value))
										});
								})),
						$author$project$Translate$Cell,
						$elm_community$maybe_extra$Maybe$Extra$isJust(model.village));
				},
				model.sector));
		var actionButton = A2(
			$elm$core$Maybe$withDefault,
			$author$project$Gizra$Html$emptyNode,
			A3(
				$elm$core$Maybe$map2,
				F2(
					function (province, district) {
						var villagePart = A2(
							$elm$core$Maybe$withDefault,
							'',
							A2(
								$elm$core$Maybe$map,
								function (geoLocation) {
									return '/' + geoLocation.name;
								},
								A2(
									$elm$core$Maybe$andThen,
									function (id) {
										return A2($pzp1997$assoc_list$AssocList$get, id, $author$project$Utils$GeoLocation$geoInfo.villages);
									},
									model.village)));
						var sectorPart = A2(
							$elm$core$Maybe$withDefault,
							'',
							A2(
								$elm$core$Maybe$map,
								function (geoLocation) {
									return '/' + geoLocation.name;
								},
								A2(
									$elm$core$Maybe$andThen,
									function (id) {
										return A2($pzp1997$assoc_list$AssocList$get, id, $author$project$Utils$GeoLocation$geoInfo.sectors);
									},
									model.sector)));
						var provincePart = A2(
							$elm$core$Maybe$withDefault,
							'',
							A2(
								$elm$core$Maybe$map,
								function ($) {
									return $.name;
								},
								A2($pzp1997$assoc_list$AssocList$get, province, $author$project$Utils$GeoLocation$geoInfo.provinces)));
						var districtPart = A2(
							$elm$core$Maybe$withDefault,
							'',
							A2(
								$elm$core$Maybe$map,
								function ($) {
									return $.name;
								},
								A2($pzp1997$assoc_list$AssocList$get, district, $author$project$Utils$GeoLocation$geoInfo.districts)));
						var cellPart = A2(
							$elm$core$Maybe$withDefault,
							'',
							A2(
								$elm$core$Maybe$map,
								function (geoLocation) {
									return '/' + geoLocation.name;
								},
								A2(
									$elm$core$Maybe$andThen,
									function (id) {
										return A2($pzp1997$assoc_list$AssocList$get, id, $author$project$Utils$GeoLocation$geoInfo.cells);
									},
									model.cell)));
						var suffix = provincePart + ('/' + (districtPart + (sectorPart + (cellPart + villagePart))));
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('actions')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$a,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$href('/admin/reports/aggregated-ncda/' + suffix)
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$button,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													A2($author$project$Translate$translate, language, $author$project$Translate$GenerateReport))
												]))
										]))
								]));
					}),
				model.province,
				model.district));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('page-content')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('header')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Please select desired view mode:')
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('inputs')
						]),
					_List_fromArray(
						[provinceInput, districtInput, sectorInput, cellInput, villageInput])),
					actionButton
				]));
	});
var $elm$core$Debug$toString = _Debug_toString;
var $author$project$Pages$Scoreboard$Model$ChaneYearGap = function (a) {
	return {$: 'ChaneYearGap', a: a};
};
var $author$project$Translate$NewSelection = {$: 'NewSelection'};
var $author$project$Translate$ANCNewborn = {$: 'ANCNewborn'};
var $author$project$Pages$Scoreboard$Model$IronDuringPregnancy = {$: 'IronDuringPregnancy'};
var $author$project$Translate$NCDAANCNewbornItemLabel = function (a) {
	return {$: 'NCDAANCNewbornItemLabel', a: a};
};
var $author$project$Pages$Scoreboard$Model$RegularCheckups = {$: 'RegularCheckups'};
var $author$project$Pages$Scoreboard$View$viewPaneHeading = F2(
	function (language, label) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('pane-heading')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(
					A2($author$project$Translate$translate, language, label))
				]));
	});
var $author$project$Translate$Month = function (a) {
	return {$: 'Month', a: a};
};
var $author$project$Translate$Status = {$: 'Status'};
var $author$project$Pages$Scoreboard$View$viewTableHeader = function (language) {
	var statusCell = A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('cell activity')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				A2($author$project$Translate$translate, language, $author$project$Translate$Status))
			]));
	var monthCells = A2(
		$elm$core$List$map,
		function (month) {
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('cell')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						A2(
							$author$project$Translate$translate,
							language,
							$author$project$Translate$Month(month)))
					]));
		},
		_List_fromArray(
			[$elm$time$Time$Jan, $elm$time$Time$Feb, $elm$time$Time$Mar, $elm$time$Time$Apr, $elm$time$Time$May, $elm$time$Time$Jun, $elm$time$Time$Jul, $elm$time$Time$Aug, $elm$time$Time$Sep, $elm$time$Time$Oct, $elm$time$Time$Nov, $elm$time$Time$Dec]));
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('table-header')
			]),
		A2($elm$core$List$cons, statusCell, monthCells));
};
var $justinmimbs$date$Date$monthToNumber = function (m) {
	switch (m.$) {
		case 'Jan':
			return 1;
		case 'Feb':
			return 2;
		case 'Mar':
			return 3;
		case 'Apr':
			return 4;
		case 'May':
			return 5;
		case 'Jun':
			return 6;
		case 'Jul':
			return 7;
		case 'Aug':
			return 8;
		case 'Sep':
			return 9;
		case 'Oct':
			return 10;
		case 'Nov':
			return 11;
		default:
			return 12;
	}
};
var $justinmimbs$date$Date$toCalendarDateHelp = F3(
	function (y, m, d) {
		toCalendarDateHelp:
		while (true) {
			var monthDays = A2($justinmimbs$date$Date$daysInMonth, y, m);
			var mn = $justinmimbs$date$Date$monthToNumber(m);
			if ((mn < 12) && (_Utils_cmp(d, monthDays) > 0)) {
				var $temp$y = y,
					$temp$m = $justinmimbs$date$Date$numberToMonth(mn + 1),
					$temp$d = d - monthDays;
				y = $temp$y;
				m = $temp$m;
				d = $temp$d;
				continue toCalendarDateHelp;
			} else {
				return {day: d, month: m, year: y};
			}
		}
	});
var $justinmimbs$date$Date$divWithRemainder = F2(
	function (a, b) {
		return _Utils_Tuple2(
			A2($justinmimbs$date$Date$floorDiv, a, b),
			A2($elm$core$Basics$modBy, b, a));
	});
var $justinmimbs$date$Date$year = function (_v0) {
	var rd = _v0.a;
	var _v1 = A2($justinmimbs$date$Date$divWithRemainder, rd, 146097);
	var n400 = _v1.a;
	var r400 = _v1.b;
	var _v2 = A2($justinmimbs$date$Date$divWithRemainder, r400, 36524);
	var n100 = _v2.a;
	var r100 = _v2.b;
	var _v3 = A2($justinmimbs$date$Date$divWithRemainder, r100, 1461);
	var n4 = _v3.a;
	var r4 = _v3.b;
	var _v4 = A2($justinmimbs$date$Date$divWithRemainder, r4, 365);
	var n1 = _v4.a;
	var r1 = _v4.b;
	var n = (!r1) ? 0 : 1;
	return ((((n400 * 400) + (n100 * 100)) + (n4 * 4)) + n1) + n;
};
var $justinmimbs$date$Date$toOrdinalDate = function (_v0) {
	var rd = _v0.a;
	var y = $justinmimbs$date$Date$year(
		$justinmimbs$date$Date$RD(rd));
	return {
		ordinalDay: rd - $justinmimbs$date$Date$daysBeforeYear(y),
		year: y
	};
};
var $justinmimbs$date$Date$toCalendarDate = function (_v0) {
	var rd = _v0.a;
	var date = $justinmimbs$date$Date$toOrdinalDate(
		$justinmimbs$date$Date$RD(rd));
	return A3($justinmimbs$date$Date$toCalendarDateHelp, date.year, $elm$time$Time$Jan, date.ordinalDay);
};
var $justinmimbs$date$Date$month = A2(
	$elm$core$Basics$composeR,
	$justinmimbs$date$Date$toCalendarDate,
	function ($) {
		return $.month;
	});
var $justinmimbs$date$Date$monthNumber = A2($elm$core$Basics$composeR, $justinmimbs$date$Date$month, $justinmimbs$date$Date$monthToNumber);
var $author$project$Pages$Scoreboard$View$formatValues = F2(
	function (currentDate, yearSelectorGap) {
		var currentMonthNumber = $justinmimbs$date$Date$monthNumber(currentDate);
		return $elm$core$List$indexedMap(
			F2(
				function (index, value) {
					return (!yearSelectorGap) ? ((_Utils_cmp(index, currentMonthNumber) < 0) ? $elm$core$String$fromInt(value) : '') : $elm$core$String$fromInt(value);
				}));
	});
var $author$project$Pages$Scoreboard$View$viewTableRow = F5(
	function (language, currentDate, yearSelectorGap, itemTransId, values) {
		var valueCells = A2(
			$elm$core$List$map,
			function (value) {
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cell value')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(value)
						]));
			},
			A3($author$project$Pages$Scoreboard$View$formatValues, currentDate, yearSelectorGap, values));
		var activityCell = A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('cell activity')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(
					A2($author$project$Translate$translate, language, itemTransId))
				]));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('table-row')
				]),
			A2($elm$core$List$cons, activityCell, valueCells));
	});
var $author$project$Pages$Scoreboard$View$viewANCNewbornPane = F4(
	function (language, currentDate, yearSelectorGap, entityType) {
		var values = function () {
			switch (entityType.$) {
				case 'EntityVillage':
					return _List_fromArray(
						[
							_List_fromArray(
							[10, 16, 13, 12, 18, 11, 14, 19, 17, 20, 15, 12]),
							_List_fromArray(
							[10, 16, 13, 12, 18, 11, 14, 19, 17, 20, 15, 12])
						]);
				case 'EntityCell':
					return _List_fromArray(
						[
							_List_fromArray(
							[105, 138, 115, 131, 122, 126, 131, 146, 133, 147, 128, 105]),
							_List_fromArray(
							[105, 138, 115, 131, 122, 126, 131, 146, 133, 147, 128, 105])
						]);
				case 'EntitySector':
					return _List_fromArray(
						[
							_List_fromArray(
							[259, 240, 212, 230, 265, 227, 211, 258, 215, 231, 274, 241]),
							_List_fromArray(
							[259, 240, 212, 230, 265, 227, 211, 258, 215, 231, 274, 241])
						]);
				default:
					return _List_fromArray(
						[
							_List_fromArray(
							[583, 557, 643, 619, 612, 632, 592, 640, 608, 562, 620, 569]),
							_List_fromArray(
							[583, 557, 643, 619, 612, 632, 592, 640, 608, 562, 620, 569])
						]);
			}
		}();
		var rows = A3(
			$elm$core$List$map2,
			F2(
				function (item, itemValues) {
					return A5(
						$author$project$Pages$Scoreboard$View$viewTableRow,
						language,
						currentDate,
						yearSelectorGap,
						$author$project$Translate$NCDAANCNewbornItemLabel(item),
						itemValues);
				}),
			_List_fromArray(
				[$author$project$Pages$Scoreboard$Model$RegularCheckups, $author$project$Pages$Scoreboard$Model$IronDuringPregnancy]),
			values);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('pane cyan')
				]),
			_List_fromArray(
				[
					A2($author$project$Pages$Scoreboard$View$viewPaneHeading, language, $author$project$Translate$ANCNewborn),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('pane-content')
						]),
					A2(
						$elm$core$List$cons,
						$author$project$Pages$Scoreboard$View$viewTableHeader(language),
						rows))
				]));
	});
var $author$project$Translate$AcuteMalnutrition = {$: 'AcuteMalnutrition'};
var $author$project$Pages$Scoreboard$Model$GoodNutrition = {$: 'GoodNutrition'};
var $author$project$Pages$Scoreboard$Model$ModerateAcuteMalnutrition = {$: 'ModerateAcuteMalnutrition'};
var $author$project$Translate$NCDAAcuteMalnutritionItemLabel = function (a) {
	return {$: 'NCDAAcuteMalnutritionItemLabel', a: a};
};
var $author$project$Pages$Scoreboard$Model$SevereAcuteMalnutrition = {$: 'SevereAcuteMalnutrition'};
var $author$project$Pages$Scoreboard$View$viewAcuteMalnutritionPane = F4(
	function (language, currentDate, yearSelectorGap, entityType) {
		var values = function () {
			switch (entityType.$) {
				case 'EntityVillage':
					return _List_fromArray(
						[
							_List_fromArray(
							[11, 17, 19, 15, 15, 7, 8, 12, 11, 17, 11, 12]),
							_List_fromArray(
							[3, 8, 2, 0, 7, 6, 1, 5, 9, 4, 2, 3]),
							_List_fromArray(
							[9, 6, 2, 8, 12, 1, 25, 3, 24, 5, 7, 11])
						]);
				case 'EntityCell':
					return _List_fromArray(
						[
							_List_fromArray(
							[98, 129, 100, 123, 112, 145, 173, 98, 145, 134, 135, 122]),
							_List_fromArray(
							[98, 98, 122, 100, 173, 173, 173, 98, 100, 100, 122, 122]),
							_List_fromArray(
							[35, 72, 98, 41, 84, 63, 52, 77, 96, 88, 55, 47])
						]);
				case 'EntitySector':
					return _List_fromArray(
						[
							_List_fromArray(
							[203, 257, 234, 245, 245, 256, 124, 145, 124, 145, 239, 240]),
							_List_fromArray(
							[203, 203, 239, 220, 256, 256, 256, 203, 220, 220, 239, 239]),
							_List_fromArray(
							[213, 243, 239, 221, 246, 236, 266, 223, 229, 221, 229, 234])
						]);
				default:
					return _List_fromArray(
						[
							_List_fromArray(
							[491, 455, 640, 678, 524, 491, 545, 640, 563, 640, 455, 491]),
							_List_fromArray(
							[530, 530, 491, 455, 640, 640, 640, 530, 455, 455, 491, 491]),
							_List_fromArray(
							[223, 569, 854, 732, 988, 622, 901, 775, 666, 444, 888, 998])
						]);
			}
		}();
		var rows = A3(
			$elm$core$List$map2,
			F2(
				function (item, itemValues) {
					return A5(
						$author$project$Pages$Scoreboard$View$viewTableRow,
						language,
						currentDate,
						yearSelectorGap,
						$author$project$Translate$NCDAAcuteMalnutritionItemLabel(item),
						itemValues);
				}),
			_List_fromArray(
				[$author$project$Pages$Scoreboard$Model$SevereAcuteMalnutrition, $author$project$Pages$Scoreboard$Model$ModerateAcuteMalnutrition, $author$project$Pages$Scoreboard$Model$GoodNutrition]),
			values);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('pane orange')
				]),
			_List_fromArray(
				[
					A2($author$project$Pages$Scoreboard$View$viewPaneHeading, language, $author$project$Translate$AcuteMalnutrition),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('pane-content')
						]),
					A2(
						$elm$core$List$cons,
						$author$project$Pages$Scoreboard$View$viewTableHeader(language),
						rows))
				]));
	});
var $author$project$Translate$AggregatedChildScoreboard = {$: 'AggregatedChildScoreboard'};
var $author$project$Translate$SelectedEntity = function (a) {
	return {$: 'SelectedEntity', a: a};
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$Pages$Scoreboard$View$viewAggregatedChildScoreboardPane = F3(
	function (language, entityName, entityType) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('pane')
				]),
			_List_fromArray(
				[
					A2($author$project$Pages$Scoreboard$View$viewPaneHeading, language, $author$project$Translate$AggregatedChildScoreboard),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('pane-content')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('selected-entity')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2(
												$author$project$Translate$translate,
												language,
												$author$project$Translate$SelectedEntity(entityType)) + ':')
										])),
									A2(
									$elm$html$Html$span,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(entityName)
										]))
								]))
						]))
				]));
	});
var $author$project$Pages$Scoreboard$Model$ChildrenUnder2 = {$: 'ChildrenUnder2'};
var $author$project$Translate$Demographics = {$: 'Demographics'};
var $author$project$Pages$Scoreboard$Model$LowBirthWeigh = {$: 'LowBirthWeigh'};
var $author$project$Translate$NCDADemographicsItemLabel = function (a) {
	return {$: 'NCDADemographicsItemLabel', a: a};
};
var $author$project$Pages$Scoreboard$Model$NewbornsThisMonth = {$: 'NewbornsThisMonth'};
var $author$project$Pages$Scoreboard$View$viewDemographicsPane = F4(
	function (language, currentDate, yearSelectorGap, entityType) {
		var values = function () {
			switch (entityType.$) {
				case 'EntityVillage':
					return _List_fromArray(
						[
							_List_fromArray(
							[12, 12, 14, 13, 15, 15, 15, 12, 13, 13, 14, 14]),
							_List_fromArray(
							[11, 11, 17, 15, 16, 16, 16, 11, 15, 15, 17, 17]),
							_List_fromArray(
							[5, 8, 6, 7, 1, 4, 3, 5, 8, 3, 1, 6])
						]);
				case 'EntityCell':
					return _List_fromArray(
						[
							_List_fromArray(
							[98, 98, 122, 100, 173, 173, 173, 98, 100, 100, 122, 122]),
							_List_fromArray(
							[97, 97, 126, 106, 176, 176, 176, 97, 102, 102, 132, 132]),
							_List_fromArray(
							[25, 34, 32, 21, 23, 34, 45, 13, 34, 56, 12, 34])
						]);
				case 'EntitySector':
					return _List_fromArray(
						[
							_List_fromArray(
							[203, 203, 239, 220, 256, 256, 256, 203, 220, 220, 239, 239]),
							_List_fromArray(
							[205, 205, 238, 227, 266, 266, 266, 205, 227, 227, 238, 238]),
							_List_fromArray(
							[145, 146, 124, 145, 124, 145, 123, 145, 134, 135, 123, 234])
						]);
				default:
					return _List_fromArray(
						[
							_List_fromArray(
							[530, 530, 491, 455, 640, 640, 640, 530, 455, 455, 491, 491]),
							_List_fromArray(
							[531, 531, 516, 455, 640, 640, 640, 531, 455, 455, 516, 516]),
							_List_fromArray(
							[345, 345, 356, 455, 214, 256, 289, 278, 267, 256, 256, 245])
						]);
			}
		}();
		var rows = A3(
			$elm$core$List$map2,
			F2(
				function (item, itemValues) {
					return A5(
						$author$project$Pages$Scoreboard$View$viewTableRow,
						language,
						currentDate,
						yearSelectorGap,
						$author$project$Translate$NCDADemographicsItemLabel(item),
						itemValues);
				}),
			_List_fromArray(
				[$author$project$Pages$Scoreboard$Model$ChildrenUnder2, $author$project$Pages$Scoreboard$Model$NewbornsThisMonth, $author$project$Pages$Scoreboard$Model$LowBirthWeigh]),
			values);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('pane cyan')
				]),
			_List_fromArray(
				[
					A2($author$project$Pages$Scoreboard$View$viewPaneHeading, language, $author$project$Translate$Demographics),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('pane-content')
						]),
					A2(
						$elm$core$List$cons,
						$author$project$Pages$Scoreboard$View$viewTableHeader(language),
						rows))
				]));
	});
var $author$project$Pages$Scoreboard$Model$HasCleanWater = {$: 'HasCleanWater'};
var $author$project$Pages$Scoreboard$Model$HasHandwashingFacility = {$: 'HasHandwashingFacility'};
var $author$project$Pages$Scoreboard$Model$HasKitchenGarden = {$: 'HasKitchenGarden'};
var $author$project$Pages$Scoreboard$Model$HasToilets = {$: 'HasToilets'};
var $author$project$Translate$InfrastructureEnvironmentWash = {$: 'InfrastructureEnvironmentWash'};
var $author$project$Pages$Scoreboard$Model$InsecticideTreatedBedNets = {$: 'InsecticideTreatedBedNets'};
var $author$project$Translate$NCDAInfrastructureEnvironmentWashItemLabel = function (a) {
	return {$: 'NCDAInfrastructureEnvironmentWashItemLabel', a: a};
};
var $author$project$Pages$Scoreboard$View$viewInfrastructureEnvironmentWashPane = F4(
	function (language, currentDate, yearSelectorGap, entityType) {
		var values = function () {
			switch (entityType.$) {
				case 'EntityVillage':
					return _List_fromArray(
						[
							_List_fromArray(
							[9, 14, 16, 12, 10, 8, 17, 11, 11, 16, 13, 15]),
							_List_fromArray(
							[13, 9, 13, 16, 12, 8, 17, 10, 10, 12, 14, 11]),
							_List_fromArray(
							[10, 9, 8, 16, 17, 11, 14, 18, 12, 15, 15, 11]),
							_List_fromArray(
							[16, 12, 11, 7, 13, 8, 16, 19, 15, 14, 11, 18]),
							_List_fromArray(
							[13, 8, 10, 9, 18, 11, 7, 17, 12, 10, 14, 17])
						]);
				case 'EntityCell':
					return _List_fromArray(
						[
							_List_fromArray(
							[118, 138, 106, 117, 123, 98, 138, 103, 125, 125, 108, 110]),
							_List_fromArray(
							[122, 92, 146, 114, 125, 128, 138, 109, 91, 118, 115, 109]),
							_List_fromArray(
							[127, 126, 130, 103, 143, 117, 121, 108, 108, 111, 136, 135]),
							_List_fromArray(
							[104, 129, 132, 100, 99, 137, 132, 110, 127, 123, 131, 119]),
							_List_fromArray(
							[116, 90, 102, 92, 115, 134, 118, 137, 92, 130, 121, 122])
						]);
				case 'EntitySector':
					return _List_fromArray(
						[
							_List_fromArray(
							[252, 244, 239, 247, 234, 259, 217, 259, 215, 250, 222, 264]),
							_List_fromArray(
							[257, 261, 209, 263, 225, 213, 226, 236, 220, 259, 240, 243]),
							_List_fromArray(
							[262, 209, 234, 237, 236, 237, 215, 267, 237, 228, 230, 256]),
							_List_fromArray(
							[252, 249, 214, 226, 284, 291, 202, 279, 238, 215, 285, 271]),
							_List_fromArray(
							[211, 262, 224, 244, 275, 237, 220, 246, 282, 265, 241, 241])
						]);
				default:
					return _List_fromArray(
						[
							_List_fromArray(
							[631, 583, 667, 626, 621, 567, 652, 611, 506, 555, 665, 636]),
							_List_fromArray(
							[537, 523, 588, 628, 617, 502, 562, 640, 504, 568, 522, 534]),
							_List_fromArray(
							[625, 623, 556, 504, 664, 655, 661, 531, 637, 558, 638, 582]),
							_List_fromArray(
							[657, 624, 577, 659, 643, 490, 532, 545, 601, 680, 506, 651]),
							_List_fromArray(
							[530, 605, 652, 621, 650, 522, 559, 606, 548, 523, 656, 492])
						]);
			}
		}();
		var rows = A3(
			$elm$core$List$map2,
			F2(
				function (item, itemValues) {
					return A5(
						$author$project$Pages$Scoreboard$View$viewTableRow,
						language,
						currentDate,
						yearSelectorGap,
						$author$project$Translate$NCDAInfrastructureEnvironmentWashItemLabel(item),
						itemValues);
				}),
			_List_fromArray(
				[$author$project$Pages$Scoreboard$Model$HasToilets, $author$project$Pages$Scoreboard$Model$HasCleanWater, $author$project$Pages$Scoreboard$Model$HasHandwashingFacility, $author$project$Pages$Scoreboard$Model$HasKitchenGarden, $author$project$Pages$Scoreboard$Model$InsecticideTreatedBedNets]),
			values);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('pane orange')
				]),
			_List_fromArray(
				[
					A2($author$project$Pages$Scoreboard$View$viewPaneHeading, language, $author$project$Translate$InfrastructureEnvironmentWash),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('pane-content')
						]),
					A2(
						$elm$core$List$cons,
						$author$project$Pages$Scoreboard$View$viewTableHeader(language),
						rows))
				]));
	});
var $author$project$Pages$Scoreboard$Model$AppropriateComplementaryFeeding = {$: 'AppropriateComplementaryFeeding'};
var $author$project$Pages$Scoreboard$Model$BreastfedSixMonths = {$: 'BreastfedSixMonths'};
var $author$project$Pages$Scoreboard$Model$DiverseDiet = {$: 'DiverseDiet'};
var $author$project$Pages$Scoreboard$Model$MealsADay = {$: 'MealsADay'};
var $author$project$Translate$NCDANutritionBehaviorItemLabel = function (a) {
	return {$: 'NCDANutritionBehaviorItemLabel', a: a};
};
var $author$project$Translate$NutritionBehavior = {$: 'NutritionBehavior'};
var $author$project$Pages$Scoreboard$View$viewNutritionBehaviorPane = F4(
	function (language, currentDate, yearSelectorGap, entityType) {
		var values = function () {
			switch (entityType.$) {
				case 'EntityVillage':
					return _List_fromArray(
						[
							_List_fromArray(
							[7, 17, 15, 19, 8, 13, 14, 11, 12, 11, 15, 12]),
							_List_fromArray(
							[8, 14, 12, 16, 18, 9, 10, 7, 13, 14, 12, 11]),
							_List_fromArray(
							[17, 18, 15, 13, 10, 7, 16, 9, 11, 17, 16, 11]),
							_List_fromArray(
							[16, 10, 11, 18, 14, 13, 12, 17, 9, 16, 15, 12])
						]);
				case 'EntityCell':
					return _List_fromArray(
						[
							_List_fromArray(
							[104, 106, 120, 120, 102, 137, 102, 139, 98, 121, 139, 126]),
							_List_fromArray(
							[120, 131, 107, 125, 135, 114, 103, 141, 127, 135, 118, 111]),
							_List_fromArray(
							[133, 116, 119, 111, 123, 144, 111, 136, 128, 115, 101, 123]),
							_List_fromArray(
							[126, 108, 147, 125, 121, 115, 121, 116, 112, 118, 121, 111])
						]);
				case 'EntitySector':
					return _List_fromArray(
						[
							_List_fromArray(
							[230, 207, 243, 232, 206, 252, 267, 211, 243, 235, 247, 230]),
							_List_fromArray(
							[240, 232, 221, 276, 261, 211, 250, 209, 287, 262, 237, 248]),
							_List_fromArray(
							[274, 290, 246, 230, 238, 231, 282, 237, 225, 279, 275, 230]),
							_List_fromArray(
							[280, 227, 236, 252, 210, 230, 232, 233, 226, 254, 249, 245])
						]);
				default:
					return _List_fromArray(
						[
							_List_fromArray(
							[567, 514, 601, 658, 557, 528, 505, 506, 540, 554, 529, 510]),
							_List_fromArray(
							[536, 567, 633, 530, 622, 583, 571, 549, 484, 497, 566, 502]),
							_List_fromArray(
							[507, 496, 609, 606, 575, 522, 548, 472, 645, 482, 483, 623]),
							_List_fromArray(
							[610, 497, 528, 582, 569, 505, 477, 567, 657, 519, 544, 568])
						]);
			}
		}();
		var rows = A3(
			$elm$core$List$map2,
			F2(
				function (item, itemValues) {
					return A5(
						$author$project$Pages$Scoreboard$View$viewTableRow,
						language,
						currentDate,
						yearSelectorGap,
						$author$project$Translate$NCDANutritionBehaviorItemLabel(item),
						itemValues);
				}),
			_List_fromArray(
				[$author$project$Pages$Scoreboard$Model$BreastfedSixMonths, $author$project$Pages$Scoreboard$Model$AppropriateComplementaryFeeding, $author$project$Pages$Scoreboard$Model$DiverseDiet, $author$project$Pages$Scoreboard$Model$MealsADay]),
			values);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('pane velvet')
				]),
			_List_fromArray(
				[
					A2($author$project$Pages$Scoreboard$View$viewPaneHeading, language, $author$project$Translate$NutritionBehavior),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('pane-content')
						]),
					A2(
						$elm$core$List$cons,
						$author$project$Pages$Scoreboard$View$viewTableHeader(language),
						rows))
				]));
	});
var $author$project$Pages$Scoreboard$Model$ModerateStunting = {$: 'ModerateStunting'};
var $author$project$Translate$NCDAStuntingItemLabel = function (a) {
	return {$: 'NCDAStuntingItemLabel', a: a};
};
var $author$project$Pages$Scoreboard$Model$NoStunting = {$: 'NoStunting'};
var $author$project$Pages$Scoreboard$Model$SevereStunting = {$: 'SevereStunting'};
var $author$project$Translate$Stunting = {$: 'Stunting'};
var $author$project$Pages$Scoreboard$View$viewStuntingPane = F4(
	function (language, currentDate, yearSelectorGap, entityType) {
		var values = function () {
			switch (entityType.$) {
				case 'EntityVillage':
					return _List_fromArray(
						[
							_List_fromArray(
							[23, 21, 17, 14, 9, 12, 18, 21, 16, 13, 19, 22]),
							_List_fromArray(
							[8, 14, 7, 18, 13, 17, 12, 15, 19, 16, 11, 10]),
							_List_fromArray(
							[19, 23, 18, 13, 15, 21, 14, 17, 22, 16, 11, 20])
						]);
				case 'EntityCell':
					return _List_fromArray(
						[
							_List_fromArray(
							[153, 129, 102, 124, 148, 115, 149, 178, 162, 148, 161, 138]),
							_List_fromArray(
							[102, 125, 136, 129, 149, 131, 125, 117, 144, 146, 137, 108]),
							_List_fromArray(
							[116, 123, 151, 135, 112, 141, 152, 126, 123, 135, 146, 148])
						]);
				case 'EntitySector':
					return _List_fromArray(
						[
							_List_fromArray(
							[270, 245, 214, 231, 265, 238, 249, 218, 221, 267, 236, 260]),
							_List_fromArray(
							[246, 269, 240, 232, 258, 215, 207, 236, 274, 252, 214, 233]),
							_List_fromArray(
							[238, 245, 214, 260, 219, 231, 241, 237, 218, 238, 255, 261])
						]);
				default:
					return _List_fromArray(
						[
							_List_fromArray(
							[605, 596, 562, 640, 621, 546, 661, 592, 635, 539, 587, 612]),
							_List_fromArray(
							[595, 581, 562, 605, 656, 576, 593, 635, 625, 655, 620, 575]),
							_List_fromArray(
							[604, 642, 553, 655, 577, 622, 600, 571, 598, 621, 542, 596])
						]);
			}
		}();
		var rows = A3(
			$elm$core$List$map2,
			F2(
				function (item, itemValues) {
					return A5(
						$author$project$Pages$Scoreboard$View$viewTableRow,
						language,
						currentDate,
						yearSelectorGap,
						$author$project$Translate$NCDAStuntingItemLabel(item),
						itemValues);
				}),
			_List_fromArray(
				[$author$project$Pages$Scoreboard$Model$SevereStunting, $author$project$Pages$Scoreboard$Model$ModerateStunting, $author$project$Pages$Scoreboard$Model$NoStunting]),
			values);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('pane velvet')
				]),
			_List_fromArray(
				[
					A2($author$project$Pages$Scoreboard$View$viewPaneHeading, language, $author$project$Translate$Stunting),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('pane-content')
						]),
					A2(
						$elm$core$List$cons,
						$author$project$Pages$Scoreboard$View$viewTableHeader(language),
						rows))
				]));
	});
var $author$project$Pages$Scoreboard$Model$ConditionalCashTransfer = {$: 'ConditionalCashTransfer'};
var $author$project$Pages$Scoreboard$Model$ConditionalFoodItems = {$: 'ConditionalFoodItems'};
var $author$project$Pages$Scoreboard$Model$FBFGiven = {$: 'FBFGiven'};
var $author$project$Translate$NCDATargetedInterventionsItemLabel = function (a) {
	return {$: 'NCDATargetedInterventionsItemLabel', a: a};
};
var $author$project$Pages$Scoreboard$Model$SupportChildWithDisability = {$: 'SupportChildWithDisability'};
var $author$project$Translate$TargetedInterventions = {$: 'TargetedInterventions'};
var $author$project$Pages$Scoreboard$Model$TreatmentForAcuteMalnutrition = {$: 'TreatmentForAcuteMalnutrition'};
var $author$project$Pages$Scoreboard$Model$TreatmentForDiarrhea = {$: 'TreatmentForDiarrhea'};
var $author$project$Pages$Scoreboard$View$viewTargetedInterventionsPane = F4(
	function (language, currentDate, yearSelectorGap, entityType) {
		var values = function () {
			switch (entityType.$) {
				case 'EntityVillage':
					return _List_fromArray(
						[
							_List_fromArray(
							[9, 15, 18, 7, 13, 11, 16, 9, 18, 8, 12, 10]),
							_List_fromArray(
							[13, 12, 7, 9, 8, 14, 17, 17, 10, 16, 10, 17]),
							_List_fromArray(
							[12, 15, 10, 18, 9, 16, 8, 11, 12, 17, 18, 14]),
							_List_fromArray(
							[3, 8, 2, 0, 7, 6, 1, 5, 9, 4, 2, 3]),
							_List_fromArray(
							[17, 12, 8, 16, 11, 10, 9, 18, 15, 7, 12, 13]),
							_List_fromArray(
							[14, 10, 18, 11, 16, 12, 13, 7, 18, 12, 9, 15])
						]);
				case 'EntityCell':
					return _List_fromArray(
						[
							_List_fromArray(
							[117, 135, 107, 104, 146, 97, 120, 138, 128, 99, 133, 128]),
							_List_fromArray(
							[139, 130, 131, 123, 103, 128, 123, 129, 145, 117, 99, 142]),
							_List_fromArray(
							[140, 96, 134, 121, 105, 98, 105, 139, 139, 138, 98, 131]),
							_List_fromArray(
							[98, 98, 122, 100, 173, 173, 173, 98, 100, 100, 122, 122]),
							_List_fromArray(
							[142, 100, 129, 117, 141, 118, 120, 120, 123, 133, 98, 137]),
							_List_fromArray(
							[110, 91, 146, 124, 133, 149, 114, 89, 107, 144, 147, 118])
						]);
				case 'EntitySector':
					return _List_fromArray(
						[
							_List_fromArray(
							[266, 288, 280, 238, 281, 275, 276, 259, 253, 246, 254, 259]),
							_List_fromArray(
							[203, 257, 234, 245, 245, 256, 124, 145, 124, 145, 239, 240]),
							_List_fromArray(
							[240, 229, 250, 240, 270, 216, 258, 247, 212, 250, 229, 209]),
							_List_fromArray(
							[203, 203, 239, 220, 256, 256, 256, 203, 220, 220, 239, 239]),
							_List_fromArray(
							[254, 261, 237, 238, 258, 249, 275, 275, 216, 239, 241, 231]),
							_List_fromArray(
							[234, 227, 255, 265, 228, 208, 234, 206, 236, 238, 231, 252])
						]);
				default:
					return _List_fromArray(
						[
							_List_fromArray(
							[582, 618, 604, 533, 550, 601, 648, 486, 503, 565, 491, 634]),
							_List_fromArray(
							[491, 455, 640, 678, 524, 491, 545, 640, 563, 640, 455, 491]),
							_List_fromArray(
							[497, 555, 484, 545, 518, 491, 537, 652, 633, 614, 616, 554]),
							_List_fromArray(
							[530, 530, 491, 455, 640, 640, 640, 530, 455, 455, 491, 491]),
							_List_fromArray(
							[620, 624, 578, 528, 530, 588, 583, 609, 625, 503, 651, 638]),
							_List_fromArray(
							[673, 635, 695, 604, 552, 618, 651, 673, 624, 586, 555, 668])
						]);
			}
		}();
		var rows = A3(
			$elm$core$List$map2,
			F2(
				function (item, itemValues) {
					return A5(
						$author$project$Pages$Scoreboard$View$viewTableRow,
						language,
						currentDate,
						yearSelectorGap,
						$author$project$Translate$NCDATargetedInterventionsItemLabel(item),
						itemValues);
				}),
			_List_fromArray(
				[$author$project$Pages$Scoreboard$Model$FBFGiven, $author$project$Pages$Scoreboard$Model$TreatmentForAcuteMalnutrition, $author$project$Pages$Scoreboard$Model$TreatmentForDiarrhea, $author$project$Pages$Scoreboard$Model$SupportChildWithDisability, $author$project$Pages$Scoreboard$Model$ConditionalCashTransfer, $author$project$Pages$Scoreboard$Model$ConditionalFoodItems]),
			values);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('pane cyan')
				]),
			_List_fromArray(
				[
					A2($author$project$Pages$Scoreboard$View$viewPaneHeading, language, $author$project$Translate$TargetedInterventions),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('pane-content')
						]),
					A2(
						$elm$core$List$cons,
						$author$project$Pages$Scoreboard$View$viewTableHeader(language),
						rows))
				]));
	});
var $author$project$Pages$Scoreboard$Model$ECDServices = {$: 'ECDServices'};
var $author$project$Pages$Scoreboard$Model$Immunization = {$: 'Immunization'};
var $author$project$Translate$NCDAUniversalInterventionItemLabel = function (a) {
	return {$: 'NCDAUniversalInterventionItemLabel', a: a};
};
var $author$project$Pages$Scoreboard$Model$OngeraMNP = {$: 'OngeraMNP'};
var $author$project$Translate$UniversalIntervention = {$: 'UniversalIntervention'};
var $author$project$Pages$Scoreboard$Model$VitaminA = {$: 'VitaminA'};
var $author$project$Pages$Scoreboard$View$viewUniversalInterventionPane = F4(
	function (language, currentDate, yearSelectorGap, entityType) {
		var values = function () {
			switch (entityType.$) {
				case 'EntityVillage':
					return _List_fromArray(
						[
							_List_fromArray(
							[13, 14, 11, 17, 9, 8, 14, 16, 12, 10, 15, 12]),
							_List_fromArray(
							[6, 10, 17, 13, 14, 12, 9, 8, 11, 15, 16, 12]),
							_List_fromArray(
							[14, 12, 16, 17, 10, 9, 13, 7, 8, 11, 19, 11]),
							_List_fromArray(
							[7, 12, 15, 10, 16, 8, 15, 17, 13, 14, 19, 11]),
							_List_fromArray(
							[12, 15, 8, 11, 11, 9, 17, 16, 15, 12, 19, 11])
						]);
				case 'EntityCell':
					return _List_fromArray(
						[
							_List_fromArray(
							[89, 98, 76, 81, 105, 92, 113, 127, 140, 115, 92, 104]),
							_List_fromArray(
							[102, 85, 121, 96, 133, 107, 127, 104, 141, 110, 88, 129]),
							_List_fromArray(
							[129, 118, 144, 96, 142, 139, 112, 131, 147, 137, 135, 123]),
							_List_fromArray(
							[136, 142, 98, 131, 143, 101, 131, 119, 144, 99, 109, 126]),
							_List_fromArray(
							[117, 121, 124, 133, 104, 141, 109, 128, 101, 137, 122, 99])
						]);
				case 'EntitySector':
					return _List_fromArray(
						[
							_List_fromArray(
							[261, 217, 205, 238, 205, 281, 276, 220, 250, 299, 283, 252]),
							_List_fromArray(
							[222, 206, 223, 196, 279, 261, 257, 216, 249, 233, 269, 248]),
							_List_fromArray(
							[241, 267, 278, 217, 211, 251, 272, 229, 240, 221, 208, 220]),
							_List_fromArray(
							[211, 248, 230, 235, 222, 240, 216, 212, 227, 262, 255, 225]),
							_List_fromArray(
							[209, 224, 215, 247, 273, 263, 258, 214, 249, 236, 275, 249])
						]);
				default:
					return _List_fromArray(
						[
							_List_fromArray(
							[597, 567, 620, 564, 485, 545, 663, 536, 498, 603, 665, 496]),
							_List_fromArray(
							[547, 599, 581, 474, 505, 489, 655, 593, 605, 539, 629, 508]),
							_List_fromArray(
							[632, 506, 551, 580, 647, 562, 508, 475, 659, 502, 642, 657]),
							_List_fromArray(
							[608, 572, 519, 648, 655, 599, 586, 547, 596, 522, 618, 484]),
							_List_fromArray(
							[567, 514, 601, 658, 557, 528, 505, 506, 540, 554, 529, 510])
						]);
			}
		}();
		var rows = A3(
			$elm$core$List$map2,
			F2(
				function (item, itemValues) {
					return A5(
						$author$project$Pages$Scoreboard$View$viewTableRow,
						language,
						currentDate,
						yearSelectorGap,
						$author$project$Translate$NCDAUniversalInterventionItemLabel(item),
						itemValues);
				}),
			_List_fromArray(
				[$author$project$Pages$Scoreboard$Model$Immunization, $author$project$Pages$Scoreboard$Model$VitaminA, $author$project$Pages$Scoreboard$Model$OngeraMNP, $author$project$Pages$Scoreboard$Model$OngeraMNP, $author$project$Pages$Scoreboard$Model$ECDServices]),
			values);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('pane orange')
				]),
			_List_fromArray(
				[
					A2($author$project$Pages$Scoreboard$View$viewPaneHeading, language, $author$project$Translate$UniversalIntervention),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('pane-content')
						]),
					A2(
						$elm$core$List$cons,
						$author$project$Pages$Scoreboard$View$viewTableHeader(language),
						rows))
				]));
	});
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$virtual_dom$VirtualDom$nodeNS = function (tag) {
	return _VirtualDom_nodeNS(
		_VirtualDom_noScript(tag));
};
var $elm$svg$Svg$node = $elm$virtual_dom$VirtualDom$nodeNS('http://www.w3.org/2000/svg');
var $author$project$Icons$iconBack = function (attrs) {
	return A3(
		$elm$svg$Svg$node,
		'svg',
		_Utils_ap(
			_List_fromArray(
				[
					A2($elm$virtual_dom$VirtualDom$attribute, 'width', '35'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'height', '30'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'viewBox', '0 0 35 30'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'fill', 'none'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'xmlns', 'http://www.w3.org/2000/svg')
				]),
			attrs),
		_List_fromArray(
			[
				A3(
				$elm$svg$Svg$node,
				'path',
				_List_fromArray(
					[
						A2($elm$virtual_dom$VirtualDom$attribute, 'd', 'M13.1449 2.50021e-06C12.9391 -8.33099e-05 12.7353 0.0463702 12.5453 0.136693C12.3552 0.227017 12.1826 0.359429 12.0374 0.526313L0.45904 13.7647C0.165096 14.1017 -1.18017e-06 14.5583 -1.14379e-06 15.0344C-1.1074e-06 15.5104 0.165096 15.9671 0.45904 16.3041L12.0374 29.5424C12.3362 29.8485 12.7261 30.012 13.127 29.9993C13.5279 29.9867 13.9094 29.7989 14.193 29.4745C14.4766 29.1501 14.6408 28.7137 14.6518 28.2551C14.6629 27.7966 14.5199 27.3505 14.2524 27.0088L3.78859 15.0384L14.2544 3.06688C14.4736 2.81592 14.623 2.49629 14.6835 2.14834C14.7441 1.80039 14.7132 1.43971 14.5947 1.11184C14.4762 0.78397 14.2755 0.503603 14.0178 0.306132C13.7601 0.10866 13.4571 0.00293216 13.1469 0.00230467L13.1449 2.50021e-06Z'),
						A2($elm$virtual_dom$VirtualDom$attribute, 'fill', 'white')
					]),
				_List_Nil),
				A3(
				$elm$svg$Svg$node,
				'path',
				_List_fromArray(
					[
						A2($elm$virtual_dom$VirtualDom$attribute, 'd', 'M32.665 13.2384L5.20109 13.2384C4.78493 13.2384 4.38579 13.4275 4.09152 13.7641C3.79725 14.1007 3.63199 14.5572 3.63199 15.0333C3.63199 15.5093 3.79725 15.9658 4.09152 16.3024C4.38579 16.639 4.78493 16.8281 5.20109 16.8281L32.665 16.8281C33.0812 16.8281 33.4803 16.639 33.7746 16.3024C34.0689 15.9658 34.2341 15.5093 34.2341 15.0333C34.2341 14.5572 34.0689 14.1007 33.7746 13.7641C33.4803 13.4275 33.0812 13.2384 32.665 13.2384Z'),
						A2($elm$virtual_dom$VirtualDom$attribute, 'fill', 'white')
					]),
				_List_Nil)
			]));
};
var $author$project$Icons$iconForward = function (attrs) {
	return A3(
		$elm$svg$Svg$node,
		'svg',
		_Utils_ap(
			_List_fromArray(
				[
					A2($elm$virtual_dom$VirtualDom$attribute, 'width', '35'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'height', '30'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'viewBox', '0 0 35 30'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'fill', 'none'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'xmlns', 'http://www.w3.org/2000/svg')
				]),
			attrs),
		_List_fromArray(
			[
				A3(
				$elm$svg$Svg$node,
				'path',
				_List_fromArray(
					[
						A2($elm$virtual_dom$VirtualDom$attribute, 'd', 'M21.0891 2.50021e-06C21.2949 -8.33099e-05 21.4987 0.0463702 21.6887 0.136693C21.8788 0.227017 22.0514 0.359429 22.1966 0.526313L33.775 13.7647C34.0689 14.1017 34.234 14.5583 34.234 15.0344C34.234 15.5104 34.0689 15.9671 33.775 16.3041L22.1966 29.5424C21.8978 29.8485 21.5079 30.012 21.107 29.9993C20.7061 29.9867 20.3246 29.7989 20.041 29.4745C19.7574 29.1501 19.5932 28.7137 19.5822 28.2551C19.5711 27.7966 19.7141 27.3505 19.9816 27.0088L30.4454 15.0384L19.9796 3.06688C19.7604 2.81592 19.611 2.49629 19.5505 2.14834C19.4899 1.80039 19.5208 1.43971 19.6393 1.11184C19.7578 0.78397 19.9586 0.503603 20.2162 0.306132C20.4739 0.10866 20.777 0.00293216 21.0871 0.00230467L21.0891 2.50021e-06Z'),
						A2($elm$virtual_dom$VirtualDom$attribute, 'fill', 'white')
					]),
				_List_Nil),
				A3(
				$elm$svg$Svg$node,
				'path',
				_List_fromArray(
					[
						A2($elm$virtual_dom$VirtualDom$attribute, 'd', 'M1.56896 13.2384L29.0329 13.2384C29.4491 13.2384 29.8482 13.4275 30.1425 13.7641C30.4367 14.1007 30.602 14.5572 30.602 15.0333C30.602 15.5093 30.4367 15.9658 30.1425 16.3024C29.8482 16.639 29.4491 16.8281 29.0329 16.8281L1.56896 16.8281C1.1528 16.8281 0.753662 16.639 0.459394 16.3024C0.165127 15.9658 -0.000137228 15.5093 -0.000137192 15.0333C-0.000137156 14.5572 0.165127 14.1007 0.459395 13.7641C0.753662 13.4275 1.1528 13.2384 1.56896 13.2384Z'),
						A2($elm$virtual_dom$VirtualDom$attribute, 'fill', 'white')
					]),
				_List_Nil)
			]));
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$Pages$Utils$viewYearSelector = F4(
	function (language, currentDate, gap, changeGapMsg) {
		var minYear = 2022;
		var forwardClass = (!gap) ? _List_fromArray(
			[
				$elm$svg$Svg$Attributes$class('hidden')
			]) : _List_Nil;
		var currentYear = $justinmimbs$date$Date$year(currentDate);
		var selectedYear = currentYear + gap;
		var backClass = _Utils_eq(selectedYear, minYear) ? _List_fromArray(
			[
				$elm$svg$Svg$Attributes$class('hidden')
			]) : _List_Nil;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('year-selector')
				]),
			_List_fromArray(
				[
					$author$project$Icons$iconBack(
					A2(
						$elm$core$List$cons,
						$elm$html$Html$Events$onClick(
							changeGapMsg(-1)),
						backClass)),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('label')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(selectedYear))
						])),
					$author$project$Icons$iconForward(
					A2(
						$elm$core$List$cons,
						$elm$html$Html$Events$onClick(
							changeGapMsg(1)),
						forwardClass))
				]));
	});
var $author$project$Pages$Scoreboard$View$viewScoreboardData = F4(
	function (language, currentDate, data, model) {
		var topBar = A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('top-bar')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('new-selection')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$a,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$href('/admin/reports/aggregated-ncda')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$button,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2($author$project$Translate$translate, language, $author$project$Translate$NewSelection))
										]))
								]))
						])),
					A4($author$project$Pages$Utils$viewYearSelector, language, currentDate, model.yearSelectorGap, $author$project$Pages$Scoreboard$Model$ChaneYearGap),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('values-percents')
						]),
					_List_Nil)
				]));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('page-content')
				]),
			_List_fromArray(
				[
					topBar,
					A3($author$project$Pages$Scoreboard$View$viewAggregatedChildScoreboardPane, language, data.entityName, data.entityType),
					A4($author$project$Pages$Scoreboard$View$viewDemographicsPane, language, currentDate, model.yearSelectorGap, data.entityType),
					A4($author$project$Pages$Scoreboard$View$viewAcuteMalnutritionPane, language, currentDate, model.yearSelectorGap, data.entityType),
					A4($author$project$Pages$Scoreboard$View$viewStuntingPane, language, currentDate, model.yearSelectorGap, data.entityType),
					A4($author$project$Pages$Scoreboard$View$viewANCNewbornPane, language, currentDate, model.yearSelectorGap, data.entityType),
					A4($author$project$Pages$Scoreboard$View$viewUniversalInterventionPane, language, currentDate, model.yearSelectorGap, data.entityType),
					A4($author$project$Pages$Scoreboard$View$viewNutritionBehaviorPane, language, currentDate, model.yearSelectorGap, data.entityType),
					A4($author$project$Pages$Scoreboard$View$viewTargetedInterventionsPane, language, currentDate, model.yearSelectorGap, data.entityType),
					A4($author$project$Pages$Scoreboard$View$viewInfrastructureEnvironmentWashPane, language, currentDate, model.yearSelectorGap, data.entityType)
				]));
	});
var $author$project$Pages$Scoreboard$View$view = F4(
	function (language, currentDate, modelBackend, model) {
		var _v0 = modelBackend.scoreboardData;
		if (_v0.$ === 'Just') {
			if (_v0.a.$ === 'Ok') {
				var data = _v0.a.a;
				return A4($author$project$Pages$Scoreboard$View$viewScoreboardData, language, currentDate, data, model);
			} else {
				var err = _v0.a.a;
				return $elm$html$Html$text(
					$elm$core$Debug$toString(err));
			}
		} else {
			return $author$project$Gizra$Html$emptyNode;
		}
	});
var $author$project$App$View$view = function (model) {
	var _v0 = model.activePage;
	switch (_v0.$) {
		case 'Menu':
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2($author$project$Error$View$view, model.language, model.errors),
						A2(
						$elm$html$Html$map,
						$author$project$App$Model$MsgMenuPage,
						A2($author$project$Pages$Menu$View$view, model.language, model.menuPage))
					]));
		case 'Scoreboard':
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2($author$project$Error$View$view, model.language, model.errors),
						A2(
						$elm$html$Html$map,
						$author$project$App$Model$MsgScoreboardPage,
						A4(
							$author$project$Pages$Scoreboard$View$view,
							model.language,
							$author$project$Gizra$NominalDate$fromLocalDateTime(model.currentTime),
							model.backend,
							model.scoreboardPage))
					]));
		default:
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Wrong page?')
					]));
	}
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{
		init: $author$project$App$Update$init,
		subscriptions: $author$project$App$Update$subscriptions,
		update: A2($Gizra$elm_fetch$Update$Fetch$andThenFetch, $author$project$App$Fetch$fetch, $author$project$App$Update$update),
		view: $author$project$App$View$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	A2(
		$elm$json$Json$Decode$andThen,
		function (page) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (appData) {
					return $elm$json$Json$Decode$succeed(
						{appData: appData, page: page});
				},
				A2($elm$json$Json$Decode$field, 'appData', $elm$json$Json$Decode$value));
		},
		A2($elm$json$Json$Decode$field, 'page', $elm$json$Json$Decode$string)))(0)}});}(this));