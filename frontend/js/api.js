class APIClient {
    constructor(baseUrl = 'http://localhost:8080/api') {
        this.baseUrl = baseUrl;
    }

    async request(endpoint, options = {}) {
        const url = `${this.baseUrl}${endpoint}`;
        const config = {
            headers: {
                'Content-Type': 'application/json',
                ...options.headers
            },
            ...options
        };

        try {
            const response = await fetch(url, config);

            if (!response.ok) {
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            return await response.json();
        } catch (error) {
            console.error(`API Error [${endpoint}]:`, error);
            throw error;
        }
    }

    // Endpoints que debes implementar en tu backend
    async checkStatus() {
        return this.request('/status');
    }

    async executeCode(code, fileName) {
        return this.request('/execute', {
            method: 'POST',
            body: JSON.stringify({ code, fileName })
        });
    }

    async saveFile(fileName, content) {
        return this.request('/files/save', {
            method: 'POST',
            body: JSON.stringify({ fileName, content })
        });
    }

    async openFile(fileName) {
        return this.request(`/files/open?fileName=${encodeURIComponent(fileName)}`);
    }

    async createFile(fileName, content = '') {
        return this.request('/files/create', {
            method: 'POST',
            body: JSON.stringify({ fileName, content })
        });
    }

    async getFiles() {
        return this.request('/files');
    }

    async deleteFile(fileName) {
        return this.request(`/files/${encodeURIComponent(fileName)}`, {
            method: 'DELETE'
        });
    }

    async getReports() {
        return this.request('/reports');
    }
}

// Instancia global del cliente API
window.apiClient = new APIClient();