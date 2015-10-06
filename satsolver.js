
var TruthValue = {};
TruthValue.FALSE = -1;
TruthValue.UNDEFINED = 0;
TruthValue.TRUE = 1;

var Literal = function (value) {
	this.value = value;
};

Literal.prototype = {

	evaluate: function (valuation) {
		var i = (this.value > 0 ? this.value : -this.value),
			isNegated = this.value < 0;

		i -= 1; // Fixing array offset

		return (!isNegated ? valuation[i] : -valuation[i]);
	},

	or: function (t, valuation) {
		var literalValue = this.evaluate(valuation);

		return Math.max(t, literalValue);
	}

}

var Clause = function (literals) {
	this.literals = literals;
};

Clause.prototype = {

	evaluate: function (valuation) {
		return this.literals.reduce(function (val, literal) {
			return literal.or(val, valuation);
		}, TruthValue.FALSE);
	},

	and: function (t, valuation) {
		var clauseValue = this.evaluate(valuation);

		return Math.min(t, clauseValue);
	}

};

var Form = function (clauses) {
	this.clauses = clauses;
	this.variables = [];
};

Form.prototype = {

	isSat: function () {
		var valuation = [];
		this.clauses.map(function (clause) {
			clause.literals.map(function (literal) {
				var i = literal.value > 0 ? literal.value : -literal.value;
				valuation[i - 1] = TruthValue.UNDEFINED;
			});
		});

		// Hard coded variable and value
		return this.step(valuation, 1, TruthValue.FALSE)
			|| this.step(valuation, 1, TruthValue.TRUE);
	},

	step: function (valuation, variable, value) {
		console.log('Stepping into (p'+ variable +', '+ value +')');

		var vs = valuation.slice();
		vs[variable - 1] = value;

		var v = this.evaluate(vs);
		// If valuation gives a truth value to the formula
		if (v != TruthValue.UNDEFINED)
			return v == TruthValue.TRUE;

		// If valuation does not give a truth value to the formula
		var variable = this.getFreeVariable(vs);
		if (variable === null)
			return true;

		return this.step(vs, variable, TruthValue.FALSE)
			|| this.step(vs, variable, TruthValue.TRUE);
	},

	evaluate: function (valuation) {
		return this.clauses.reduce(function (prev, clause) {
			return clause.and(prev, valuation);
		}, TruthValue.TRUE);
	},

	getFreeVariable: function (valuation) {
		for (var i in valuation)
			if (valuation[i] == TruthValue.UNDEFINED)
				return parseInt(i) + 1;

		return null;
	}

};

var f = new Form([
	new Clause([new Literal(1)]),
	new Clause([new Literal(-1), new Literal(3)])
]);

console.log(f.isSat());
