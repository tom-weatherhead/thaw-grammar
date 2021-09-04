// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/utilities.ts

const strTrue = 'λx.λy.x';
const strFalse = 'λx.λy.y';

const strIf = 'λb.λx.λy.((b x) y)'; // if b then x else y

export const mapLCExprNamesToStrings = new Map<string, string>();

mapLCExprNamesToStrings.set('identity', 'λx.x');

mapLCExprNamesToStrings.set('true', strTrue);
mapLCExprNamesToStrings.set('false', strFalse);

mapLCExprNamesToStrings.set('0', 'λf.λx.x');
mapLCExprNamesToStrings.set('1', 'λf.λx.(f x)');
mapLCExprNamesToStrings.set('2', 'λf.λx.(f (f x))');
mapLCExprNamesToStrings.set('3', 'λf.λx.(f (f (f x)))');
mapLCExprNamesToStrings.set('4', 'λf.λx.(f (f (f (f x))))');
mapLCExprNamesToStrings.set('5', 'λf.λx.(f (f (f (f (f x)))))');
mapLCExprNamesToStrings.set('6', 'λf.λx.(f (f (f (f (f (f x))))))');
mapLCExprNamesToStrings.set('7', 'λf.λx.(f (f (f (f (f (f (f x)))))))');
mapLCExprNamesToStrings.set('8', 'λf.λx.(f (f (f (f (f (f (f (f x))))))))');
mapLCExprNamesToStrings.set('9', 'λf.λx.(f (f (f (f (f (f (f (f (f x)))))))))');

mapLCExprNamesToStrings.set('isZero', `λn.((n λx.${strFalse}) ${strTrue})`);
mapLCExprNamesToStrings.set('successor', 'λn.λf.λx.(f ((n f) x))');
mapLCExprNamesToStrings.set('predecessor', 'λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)');
mapLCExprNamesToStrings.set('if', strIf);
mapLCExprNamesToStrings.set('and', `λp.λq.((p q) ${strFalse})`);
mapLCExprNamesToStrings.set('or', `λp.λq.(((${strIf} p) ${strTrue}) q)`);
mapLCExprNamesToStrings.set('add', 'λm.λn.λf.λx.((n f) ((m f) x))');
mapLCExprNamesToStrings.set('multiply', 'λm.λn.λf.(m (n f))');
// mapLCExprNamesToStrings.set('', '');

// export const mapCombinatorNamesToStrings = new Map<string, string>();
//
// mapCombinatorNamesToStrings.set('I', 'λx.x');
// mapCombinatorNamesToStrings.set('', '');
