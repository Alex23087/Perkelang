import * as vscode from 'vscode';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { execSync } from 'child_process';

export function activate(context: vscode.ExtensionContext) {
    const diagnosticCollection = vscode.languages.createDiagnosticCollection('Perkelang');

    vscode.workspace.onDidSaveTextDocument((document) => {
        if (document.languageId === 'perkelang') {
            const diagnostics: vscode.Diagnostic[] = [];

            // Run your compiler and get errors
            const errors = runCompiler(document.getText());

            errors.forEach((error) => {
                // Ensure the range is valid
                const startLine = Math.max(0, error.startLine - 1);
                const endLine = Math.max(0, error.endLine - 1);
                const range = new vscode.Range(
                    new vscode.Position(startLine, error.characterStart),
                    new vscode.Position(endLine, error.characterEnd)
                );
                diagnostics.push(new vscode.Diagnostic(range, `${error.errorType}: ${error.message}`, vscode.DiagnosticSeverity.Error));
            });

            // Set diagnostics or clear them if no errors
            if (diagnostics.length > 0) {
                diagnosticCollection.set(document.uri, diagnostics);
            } else {
                diagnosticCollection.set(document.uri, []); // Clear diagnostics
            }
        }
    });

    context.subscriptions.push(diagnosticCollection);
}

export function deactivate() { }

function runCompiler(sourceCode: string): { errorType: string; startLine: number; characterStart: number; endLine: number; characterEnd: number; message: string }[] {
    const errors: { errorType: string; startLine: number; characterStart: number; endLine: number; characterEnd: number; message: string }[] = [];

    // Create a temporary file to store the source code
    const tmpDir: string = os.tmpdir();
    const tmpFile: string = path.join(tmpDir, `source-${Date.now()}.tmp`);

    fs.writeFileSync(tmpFile, sourceCode, 'utf8');

    // Example shell command to run a compiler
    const command = `perkc --check ${tmpFile}`;

    // Wait for the command to finish
    try {
        const result = execSync(command, { encoding: 'utf8' });
        if (result !== '') {
            const parsedError = JSON.parse(result);
            errors.push({
                errorType: parsedError.error,
                startLine: parsedError.start_line,
                endLine: parsedError.end_line,
                characterStart: parsedError.start_col,
                characterEnd: parsedError.end_col,
                message: parsedError.message,
            });
        }
    } catch (execError: any) {
        vscode.window.showErrorMessage(`Error executing command: ${execError.message}`);
    }

    // Clean up the temporary file
    try {
        fs.unlinkSync(tmpFile);
    } catch (unlinkError: any) {
        vscode.window.showWarningMessage(`Failed to delete temporary file: ${unlinkError.message}`);
    }

    return errors;
}