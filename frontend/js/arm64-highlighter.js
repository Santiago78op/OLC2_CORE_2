class ARM64SyntaxHighlighter {
    constructor() {
        // ARM64 instruction patterns
        this.patterns = {
            // Sections and directives
            section: /^(\s*)(\.(?:text|data|bss|section|global|align|ascii|asciz|quad|word|byte))\b/gm,
            
            // Labels
            label: /^([a-zA-Z_][a-zA-Z0-9_]*):(?!\w)/gm,
            
            // Comments
            comment: /(\/\/.*$|;.*$)/gm,
            
            // Strings
            string: /"([^"\\]|\\.)*"/g,
            
            // Branch instructions
            branch: /\b(b|bl|br|blr|ret|b\.eq|b\.ne|b\.lt|b\.le|b\.gt|b\.ge|b\.cs|b\.cc|b\.mi|b\.pl|b\.vs|b\.vc|b\.hi|b\.ls|b\.al|b\.nv|beq|bne|blt|ble|bgt|bge|bcs|bcc|bmi|bpl|bvs|bvc|bhi|bls|bal|bnv)\b/gi,
            
            // Data movement instructions
            instruction: /\b(mov|movz|movk|movn|ldr|ldrb|ldrh|ldrsb|ldrsh|ldrsw|str|strb|strh|stp|ldp|adr|adrp)\b/gi,
            
            // Arithmetic instructions
            arithmetic: /\b(add|adds|sub|subs|mul|smull|umull|div|sdiv|udiv|rem|and|orr|eor|bic|lsl|lsr|asr|ror)\b/gi,
            
            // Comparison instructions
            comparison: /\b(cmp|cmn|tst|ccmp|ccmn)\b/gi,
            
            // System instructions
            system: /\b(nop|svc|hvc|smc|brk|hlt|isb|dmb|dsb|mrs|msr)\b/gi,
            
            // Registers
            register: /\b(x[0-9]|x[1-2][0-9]|x30|w[0-9]|w[1-2][0-9]|w30|sp|xzr|wzr|lr|pc)\b/gi,
            
            // Immediate values
            immediate: /#(-?(?:0x[0-9a-fA-F]+|[0-9]+))/g,
            
            // Numbers
            number: /\b(?:0x[0-9a-fA-F]+|\d+)\b/g,
            
            // Memory addressing
            memory: /\[(x[0-9]+|x[1-2][0-9]|x30|sp)(?:\s*,\s*#?-?\d+)?\]/gi,
            
            // Symbols and identifiers
            symbol: /\b[a-zA-Z_][a-zA-Z0-9_]*\b/g
        };
        
        // Order matters for proper highlighting
        this.highlightOrder = [
            'comment',
            'string', 
            'section',
            'label',
            'branch',
            'instruction',
            'arithmetic', 
            'comparison',
            'system',
            'memory',
            'register',
            'immediate',
            'number'
        ];
    }

    // Main highlight method for display purposes only
    highlight(code) {
        if (!code || typeof code !== 'string') {
            return '';
        }

        let highlightedCode = this.escapeHtml(code);
        
        // Apply syntax highlighting in order
        for (const patternName of this.highlightOrder) {
            if (this.patterns[patternName]) {
                highlightedCode = this.applyPattern(highlightedCode, patternName, this.patterns[patternName]);
            }
        }
        
        return highlightedCode;
    }

    applyPattern(code, className, pattern) {
        return code.replace(pattern, (match, ...groups) => {
            // Handle different pattern types
            switch (className) {
                case 'section':
                    return `${groups[0]}<span class="asm-directive">${groups[1]}</span>`;
                case 'label':
                    return `<span class="asm-label">${groups[0]}</span>:`;
                case 'string':
                    return `<span class="asm-string">${match}</span>`;
                case 'branch':
                    return `<span class="asm-branch">${match}</span>`;
                case 'instruction':
                case 'arithmetic':
                case 'comparison':
                case 'system':
                    return `<span class="asm-instruction">${match}</span>`;
                case 'register':
                    return `<span class="asm-register">${match}</span>`;
                case 'immediate':
                    return `<span class="asm-immediate">${match}</span>`;
                case 'number':
                    return `<span class="asm-number">${match}</span>`;
                case 'memory':
                    return `<span class="asm-memory">${match}</span>`;
                default:
                    return match;
            }
        });
    }

    // Helper to escape HTML
    escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    // Strip HTML tags from highlighted code
    stripHtml(htmlCode) {
        const div = document.createElement('div');
        div.innerHTML = htmlCode;
        return div.textContent || div.innerText || '';
    }

    // Add line numbers for display
    addLineNumbers(code) {
        const lines = code.split('\n');
        const numberedLines = lines.map((line, index) => {
            const lineNumber = (index + 1).toString().padStart(3, ' ');
            return { number: lineNumber, content: line };
        });
        
        return numberedLines;
    }

    // Format code with line numbers for display
    formatWithLineNumbers(code) {
        const highlightedCode = this.highlight(code);
        const lines = highlightedCode.split('\n');
        
        const lineNumbers = lines.map((_, index) => 
            (index + 1).toString().padStart(3, ' ')
        ).join('\n');
        
        return {
            lineNumbers: lineNumbers,
            code: highlightedCode
        };
    }

    // Method to get instruction info on hover (for tooltips)
    getInstructionInfo(instruction) {
        const instructionMap = {
            'mov': 'Move data between registers',
            'ldr': 'Load register from memory',
            'str': 'Store register to memory',
            'add': 'Add two values',
            'sub': 'Subtract two values',
            'cmp': 'Compare two values',
            'b': 'Branch unconditionally',
            'bl': 'Branch with link (function call)',
            'ret': 'Return from function',
            // Add more as needed
        };
        
        return instructionMap[instruction.toLowerCase()] || 'ARM64 instruction';
    }
    
    // Method for processing code that will be saved as an assembly file
    // This returns the ORIGINAL code without any HTML formatting
    getPlainCode(code) {
        // Check if the code already has HTML tags (was previously highlighted)
        if (code.includes('<span class="asm-')) {
            // Strip HTML tags to get plain code
            return this.stripHtml(code);
        }
        // If no HTML tags, it's already plain
        return code;
    }

    // New method to ensure we get properly formatted assembly code
    getAssemblyCode(code) {
        // Check if the code already has HTML tags (was previously highlighted)
        if (code.includes('<span') || code.includes('"asm-')) {
            // Strip HTML tags to get plain code
            return this.stripHtml(code);
        }
        // If no HTML tags, it's already plain
        return code;
    }
    
    // Mejorar la función stripHtml para manejar casos especiales
    stripHtml(htmlCode) {
        // Primero limpiar casos especiales como "asm-comment">
        htmlCode = htmlCode
            .replace(/"asm-[^"]*">/g, '')
            .replace(/class="[^"]*"/g, '');
            
        // Luego usar el DOM para limpiar el resto
        const div = document.createElement('div');
        div.innerHTML = htmlCode;
        return div.textContent || div.innerText || '';
    }
}

// Make available globally
window.ARM64SyntaxHighlighter = ARM64SyntaxHighlighter;