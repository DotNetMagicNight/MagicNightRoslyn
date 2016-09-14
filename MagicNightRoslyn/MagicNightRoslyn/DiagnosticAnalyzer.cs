using System.Collections.Immutable;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace MagicNightRoslyn {

    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MagicNightRoslynAnalyzer : DiagnosticAnalyzer {
        public const string DiagnosticId = "MagicNightRoslyn";

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization
        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Naming";

        private static DiagnosticDescriptor Rule = new DiagnosticDescriptor(
            DiagnosticId, 
            Title, 
            MessageFormat, 
            Category, 
            DiagnosticSeverity.Warning, 
            isEnabledByDefault: true, 
            description: Description
        );

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context) {

            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            context.RegisterSyntaxNodeAction(AnalyzeInvocation, SyntaxKind.InvocationExpression);
        }

        private static void AnalyzeInvocation(SyntaxNodeAnalysisContext context) {
            var invocationNode = (InvocationExpressionSyntax)context.Node;
            var symbol = context.SemanticModel.GetSymbolInfo(invocationNode).Symbol as IMethodSymbol;
            if(symbol == null) {
                return;
            }
            var attributes = symbol.GetAttributes();
            if(!attributes.Any()) {
                return;
            }
            var formatAttribute = attributes.FirstOrDefault(attr => attr.AttributeClass.OriginalDefinition.ContainingAssembly.Name == "StringFormatAttribute"
                                                                    && attr.AttributeClass.OriginalDefinition.ContainingNamespace.Name == "StringFormatAttribute"
                                                                    && attr.AttributeClass.OriginalDefinition.Name == "StringFormat");
            if(formatAttribute == null) {
                return;
            }
            var args = formatAttribute.ConstructorArguments.ToArray();
            var formatArg = args[0].Value;
            var argsArg = args[1].Value;

            // check the method declaration to see which parameters are the ones we care about positionally
            var parameters = symbol.Parameters;
            int formatIdx = -1;
            int argsIdx = -1;
            for(var i = 0; i < parameters.Length; i++) {
                if(formatArg.Equals(parameters[i].Name)) {
                    formatIdx = i;
                }
                if(argsArg.Equals(parameters[i].Name)) {
                    argsIdx = i;
                }
            }
            if(formatIdx == -1) {
                ReportDiagnostic(context, invocationNode.GetLocation(), "Parameter not found", $"Parameter {formatArg} was not found");
                return;
            }
            if(argsIdx == -1) {
                ReportDiagnostic(context, invocationNode.GetLocation(), "Parameter not found", $"Parameter {argsArg} was not found");
                return;
            }

            // check the invocation to see what it is being called with!
            var formatArgument = invocationNode.ArgumentList.Arguments[formatIdx];
            var argsArgument = invocationNode.ArgumentList.Arguments.Skip(argsIdx).ToArray();
            
            // we have found a format attribute, let's investigate the parameters!
            if(formatArgument.Expression.IsKind(SyntaxKind.StringLiteralExpression)) {
                var formatString = formatArgument.Expression as LiteralExpressionSyntax;
                ValidateFormatString(formatString.Token.ValueText, argsArgument, context, invocationNode.GetLocation());
            }
        }

        private static void ValidateFormatString(string format, ArgumentSyntax[] args, SyntaxNodeAnalysisContext context, Location location) {
            var matches = Regex.Matches(format, "{[0-9]*}");
            if(args.Length != matches.Count) {
                ReportDiagnostic(context, location, "Invalid number of parameters", $"The number of parameters does not match the number of arguments");
            }
        }

        private static void ReportDiagnostic(SyntaxNodeAnalysisContext context, Location location, string title, string messageFormat) {
            var diagnostic = Diagnostic.Create(new DiagnosticDescriptor(
                DiagnosticId,
                title,
                messageFormat,
                Category,
                DiagnosticSeverity.Error,
                isEnabledByDefault: true,
                description: Description
            ), location, "");
            context.ReportDiagnostic(diagnostic);
        }
    }
}
